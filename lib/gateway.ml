open Util

type consumer_cast_msg = [ `Event of Event.t ]
type consumer = consumer_cast_msg Actaa.Gen_server.t_cast

type init_arg = {
  consumer : consumer;
  token : string;
  intents : int;
  state : State.t;
}

type call_msg = |
type call_reply = |

type voice_state = {
  guild_id : string;
  channel_id : string option;
  self_mute : bool;
  self_deaf : bool;
}

type cast_msg = [ `VoiceStateUpdate of voice_state | `ForceResume ]
type basic_msg = (call_msg, call_reply, cast_msg) Actaa.Gen_server.basic_msg
type msg = [ basic_msg | `Timeout of [ `Heartbeat ] | Ws.Process.msg ]

type state = {
  consumer : consumer;
  ws_conn : Ws.conn;
  heartbeat_interval : float option;
  st : State.t;
  token : string;
  intents : int;
  resume : (string (* resume_gateway_url *) * string (* session_id *)) option;
  seq : int;
  identify_sent : bool;
}
[@@deriving make]

type Actaa.Process.Stop_reason.t += Restart

let send_json conn json =
  let content = Yojson.Safe.to_string json in
  Logs.info (fun m -> m "Sending: %s" content);
  Ws.write conn Websocket.Frame.(create ~opcode:Opcode.Text ~content ())

let send_heartbeat seq conn =
  let open Event in
  Heartbeat (Some seq) |> to_yojson |> send_json conn

let extract_sequence_number src =
  let open Yojson.Safe.Util in
  src |> member "s" |> to_int_option

class t =
  object (self)
    inherit [init_arg, msg, state] Actaa.Gen_server.behaviour

    method connect_ws env ~sw =
      let endpoint = "https://gateway.discord.gg/?v=10&encoding=json" in
      let conn = Ws.connect ~sw env endpoint in
      Ws.Process.start ~sw conn (self :> _ Actaa.Process.t1);
      conn

    method resume_ws env ~sw state =
      (* Resume *)
      let resume_gateway_url, session_id = Option.get state.resume in
      let url =
        let u = Uri.of_string resume_gateway_url in
        let u = Uri.with_scheme u (Some "https") in
        let u = Uri.with_path u "/" in
        let u =
          Uri.with_query u [ ("v", [ "10" ]); ("encoding", [ "json" ]) ]
        in
        Uri.to_string u
      in
      Logs.info (fun m -> m "Resuming: %s" url);
      let conn = Ws.connect ~sw env url in
      Ws.Process.start ~sw conn (self :> _ Actaa.Process.t1);

      (* Send Resume event *)
      Event.(
        Resume { token = state.token; session_id; seq = state.seq } |> to_yojson)
      |> send_json conn;

      { state with ws_conn = conn }

    method private init env ~sw { consumer; token; intents; state = st } =
      let ws_conn = self#connect_ws env ~sw in
      make_state ~consumer ~ws_conn ~token ~intents ~st ~seq:0
        ~identify_sent:false ()

    method! private terminate _ ~sw:_ _state _reason = ()

    method handle_event env ~sw state =
      let open Event in
      function
      | Hello { heartbeat_interval } ->
          let interval = float_of_int heartbeat_interval /. 1000.0 in
          if Option.is_none state.heartbeat_interval then
            Actaa.Timer.spawn env ~sw ~seconds:interval ~id:`Heartbeat
              ~target:(self :> _ Actaa.Timer.receiver)
            |> ignore;

          if not state.identify_sent then
            Identify
              {
                token = state.token;
                intents = state.intents;
                properties =
                  `Assoc
                    [
                      ("os", `String "linux");
                      ("browser", `String "yomer");
                      ("device", `String "yomer");
                    ];
              }
            |> to_yojson |> send_json state.ws_conn;

          {
            state with
            heartbeat_interval = Some interval;
            identify_sent = true;
          }
      | HeartbeatAck ->
          (* FIXME: check if HeartbeatAck is correctly received *)
          Logs.info (fun m -> m "Heartbeat acked");
          state
      | Heartbeat _ ->
          Heartbeat (Some state.seq) |> to_yojson |> send_json state.ws_conn;
          state
      | Dispatch
          (VOICE_STATE_UPDATE
            ({ guild_id = Some guild_id; user_id; session_id; channel_id; _ } as
             payload)) ->
          let self_user_id = State.me state.st |> Option.get in
          (if user_id = self_user_id.id then
             match channel_id with
             | Some channel_id ->
                 (* Start vgw if not started *)
                 let vgw = Voice_gateway.create () in
                 if
                   State.set_voice_if_not_exists state.st ~guild_id ~channel_id
                     ~gateway:vgw
                 then Voice_gateway.start vgw env sw state.consumer ~guild_id;
                 (* Attach voice state *)
                 let vgw = State.voice state.st guild_id |> Option.get in
                 Voice_gateway.attach_voice_state ~user_id ~session_id vgw
             | None ->
                 (* Stop vgw if already started *)
                 State.voice state.st guild_id |> Option.iter Voice_gateway.stop);

          State.set_voice_states state.st ~guild_id ~user_id payload;

          state
      | Dispatch (VOICE_SERVER_UPDATE { token; guild_id; endpoint }) ->
          let v = State.voice state.st guild_id |> Option.get in
          Voice_gateway.attach_voice_server ~token ~endpoint v;
          state
      | Dispatch (READY { user; resume_gateway_url; session_id; _ }) ->
          State.set_me state.st user;
          { state with resume = Some (resume_gateway_url, session_id) }
      | Dispatch (GUILD_CREATE { voice_states; id = guild_id; _ }) ->
          voice_states
          |> Option.iter
               (State.initialize_guild_voice_states state.st ~guild_id);
          state
      | Reconnect | InvalidSession true -> self#resume_ws env ~sw state
      | Dispatch _ | VoiceStateUpdate _ | InvalidSession false -> state
      | Identify _ | Resume _ | VoiceReady _ | VoiceSpeaking _ ->
          failwith "Unexpected event"

    method! private handle_info env ~sw state =
      function
      | #basic_msg -> assert false
      | `Timeout `Heartbeat ->
          send_heartbeat state.seq state.ws_conn;
          Actaa.Timer.spawn env ~sw
            ~seconds:(Option.get state.heartbeat_interval)
            ~id:`Heartbeat
            ~target:(self :> _ Actaa.Timer.receiver)
          |> ignore;
          `NoReply state
      | `WSText (content, _) -> (
          try
            let json = Yojson.Safe.from_string content in
            let state =
              match extract_sequence_number json with
              | None -> state
              | Some seq -> { state with seq }
            in
            let ev = Event.of_yojson json in
            let state = self#handle_event env ~sw state ev in
            Actaa.Gen_server.cast state.consumer (`Event ev);
            `NoReply state
          with e ->
            Logs.err (fun m ->
                m "Handling event failed: %s: %s: %s" (Printexc.to_string e)
                  content
                  (Printexc.get_backtrace ()));
            `NoReply state)
      | `WSClose (_, conn) when Ws.id state.ws_conn = Ws.id conn ->
          Logs.info (fun m ->
              m "Gateway WS connection closed. Trying to resume.");
          `NoReply (self#resume_ws env ~sw state)
      | `WSClose _ ->
          Logs.info (fun m -> m "Ignoring gateway WS connection closing");
          `NoReply state

    method! private handle_cast env ~sw state =
      function
      | `VoiceStateUpdate { guild_id; channel_id; self_mute; self_deaf } ->
          Event.VoiceStateUpdate { guild_id; channel_id; self_mute; self_deaf }
          |> Event.to_yojson |> send_json state.ws_conn;
          `NoReply state
      | `ForceResume ->
          let state = self#resume_ws env ~sw state in
          `NoReply state
  end

let spawn env ~sw ~token ~intents ~state ~consumer =
  let t = new t in
  Actaa.Gen_server.start env ~sw { token; intents; state; consumer } t;
  t

let send_voice_state_update ~guild_id ?channel_id ~self_mute ~self_deaf t =
  Actaa.Gen_server.cast t
    (`VoiceStateUpdate { guild_id; channel_id; self_mute; self_deaf })

let force_resume t = Actaa.Gen_server.cast t `ForceResume
