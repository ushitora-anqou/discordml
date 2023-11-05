let encryption_mode = "xsalsa20_poly1305"

type consumer_cast_msg = [ `Event of Event.t ]
type consumer = consumer_cast_msg Actaa.Gen_server.t_cast
type init_arg = { guild_id : string; consumer : consumer }
type call_msg = |
type call_reply = |
type voice_state = { user_id : string; session_id : string }
type voice_server = { token : string; endpoint : string }

type cast_msg =
  [ `VoiceState of voice_state
  | `VoiceServer of voice_server
  | `FrameSource of Eio.Flow.source_ty Eio.Resource.t
  | `Speaking of int (* ssrc *) * bool (* speaking *)
  | `Stop ]

type basic_msg = (call_msg, call_reply, cast_msg) Actaa.Gen_server.basic_msg
type msg = [ basic_msg | `Timeout of [ `Heartbeat ] | Ws.Process.msg ]
type process_status = WaitingParameters | Running

type state = {
  guild_id : string;
  consumer : consumer;
  udp_stream : Voice_udp_stream.t;
  voice_state : voice_state option;
  voice_server : voice_server option;
  status : process_status; [@default WaitingParameters]
  ws_conn : Ws.conn option;
  heartbeat_interval : float option;
}
[@@deriving make]

type Actaa.Process.Stop_reason.t += Restart

let send_json conn json =
  let content = Yojson.Safe.to_string json in
  Logs.info (fun m -> m "Sending: %s" content);
  Ws.write conn Websocket.Frame.(create ~opcode:Opcode.Text ~content ())

let send_select_protocol conn ip port modes =
  if modes |> List.find_opt (fun m -> m = encryption_mode) = None then
    failwith (encryption_mode ^ " is not supported");
  Voice_event.(
    SelectProtocol
      {
        protocol = "udp";
        data = { address = ip; port; mode = encryption_mode };
      }
    |> to_yojson)
  |> send_json conn

let send_heartbeat clock conn =
  let nonce = Eio.Time.now clock |> int_of_float in
  Voice_event.(Heartbeat nonce |> to_yojson) |> send_json conn;
  Logs.info (fun m -> m "Voice Heartbeat sent")

class t =
  object (self)
    inherit [init_arg, msg, state] Actaa.Gen_server.behaviour

    method private init _env ~sw:_ { guild_id; consumer } =
      let udp_stream = Voice_udp_stream.create () in
      make_state ~guild_id ~consumer ~udp_stream ()

    method! private terminate _ ~sw:_ state _reason =
      Voice_udp_stream.close state.udp_stream;
      ()

    method private connect_ws env ~sw state =
      let endpoint = (Option.get state.voice_server).endpoint in
      let conn =
        Ws.connect ~sw env ("https://" ^ endpoint ^ "/?v=4&encoding=json")
      in
      Ws.Process.start ~sw conn (self :> _ Actaa.Process.t1);
      conn

    method private start_running env ~sw state =
      let token = (Option.get state.voice_server).token in
      let user_id = (Option.get state.voice_state).user_id in
      let session_id = (Option.get state.voice_state).session_id in

      let conn = self#connect_ws env ~sw state in

      Voice_event.(
        Identify { server_id = state.guild_id; user_id; session_id; token }
        |> to_yojson)
      |> send_json conn;

      { state with status = Running; ws_conn = Some conn }

    method private start_running_if_ready env ~sw state =
      if
        Option.is_some state.voice_server
        && Option.is_some state.voice_state
        && state.status = WaitingParameters
      then self#start_running env ~sw state
      else state

    method private handle_voice_event env ~sw state =
      function
      | Voice_event.Hello { heartbeat_interval } ->
          let interval = heartbeat_interval /. 1000.0 in
          if Option.is_none state.heartbeat_interval then
            Actaa.Timer.spawn env ~sw ~seconds:interval ~id:`Heartbeat
              ~target:(self :> _ Actaa.Timer.receiver)
            |> ignore;
          { state with heartbeat_interval = Some interval }
      | Resumed | HeartbeatAck _ -> state
      | Ready { ip; port; ssrc; modes; _ } ->
          let vgw = (self :> Voice_udp_stream.vgw) in
          Voice_udp_stream.connect env sw state.udp_stream
            { vgw; ip; port; ssrc; modes };
          let my_addr, my_port =
            Voice_udp_stream.discover_ip state.udp_stream
          in
          let ws_conn = Option.get state.ws_conn in
          send_select_protocol ws_conn my_addr my_port modes;
          state
      | SessionDescription { secret_key; _ } ->
          Voice_udp_stream.attach_secret_key state.udp_stream secret_key;
          Actaa.Gen_server.cast state.consumer
            (`Event (Event.VoiceReady { guild_id = state.guild_id }));
          state
      | Identify _ | SelectProtocol _ | Speaking _ | Resume _ | Heartbeat _ ->
          failwith "Unexpected event"
      | Unknown_13 | Unknown_18 | Unknown_20 -> state

    method! private handle_info env ~sw state =
      function
      | #basic_msg -> assert false
      | `Timeout `Heartbeat ->
          let clock = Eio.Stdenv.clock env in
          send_heartbeat clock (Option.get state.ws_conn);
          Actaa.Timer.spawn env ~sw
            ~seconds:(Option.get state.heartbeat_interval)
            ~id:`Heartbeat
            ~target:(self :> _ Actaa.Timer.receiver)
          |> ignore;
          `NoReply state
      | `WSText (content, _) -> (
          try
            content |> Yojson.Safe.from_string |> Voice_event.of_yojson
            |> self#handle_voice_event env ~sw state
            |> fun state -> `NoReply state
          with e ->
            Logs.err (fun m ->
                m "Handling event failed (voice): %s: %s" (Printexc.to_string e)
                  content);
            `NoReply state)
      | `WSClose (`Status_code 4006, _) ->
          (* FIXME: get a new session *)
          Logs.err (fun m -> m "voice gateway: ws closed with status code 4006");
          `Stop (Actaa.Process.Stop_reason.Normal, state)
      | `WSClose _ ->
          Logs.info (fun m -> m "voice gateway: ws closed. Trying to resume.");
          let ws_conn = self#connect_ws env ~sw state in
          let server_id = state.guild_id in
          let token = (Option.get state.voice_server).token in
          let session_id = (Option.get state.voice_state).session_id in
          Voice_event.(Resume { server_id; session_id; token } |> to_yojson)
          |> send_json ws_conn;
          `NoReply { state with ws_conn = Some ws_conn }

    method! private handle_cast env ~sw state =
      function
      | `VoiceState x ->
          let state = { state with voice_state = Some x } in
          `NoReply (self#start_running_if_ready env ~sw state)
      | `VoiceServer x ->
          let state = { state with voice_server = Some x } in
          `NoReply (self#start_running_if_ready env ~sw state)
      | `FrameSource src ->
          Voice_udp_stream.send_frame_source state.udp_stream src;
          `NoReply state
      | `Speaking (ssrc, speaking) ->
          let send_json conn json =
            let content = Yojson.Safe.to_string json in
            Logs.info (fun m -> m "Sending: %s" content);
            Ws.write conn
              Websocket.Frame.(create ~opcode:Opcode.Text ~content ())
          in
          Voice_event.(
            Speaking
              { speaking = (if speaking then 1 else 0); delay = Some 0; ssrc }
            |> to_yojson)
          |> send_json (Option.get state.ws_conn);
          Actaa.Gen_server.cast state.consumer
            (`Event
              (Event.VoiceSpeaking { guild_id = state.guild_id; speaking }));
          `NoReply state
      | `Stop ->
          Logs.info (fun m -> m "voice gateway: stopping normally");
          `Stop (Actaa.Process.Stop_reason.Normal, state)
  end

let create () = new t

let start t env sw consumer ~guild_id =
  Actaa.Gen_server.start env ~sw { guild_id; consumer } t

let stop t = Actaa.Gen_server.cast t `Stop

let attach_voice_state ~user_id ~session_id t =
  Actaa.Gen_server.cast t (`VoiceState { user_id; session_id })

let attach_voice_server ~token ~endpoint t =
  Actaa.Gen_server.cast t (`VoiceServer { token; endpoint })

let send_frame_source t src = Actaa.Gen_server.cast t (`FrameSource src)
