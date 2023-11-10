type 'user_state user_handler =
  Eio_unix.Stdenv.base ->
  sw:Eio.Switch.t ->
  Agent.t ->
  Rest.t ->
  'user_state ->
  Event.t ->
  'user_state

type 'user_state init_arg = {
  token : string;
  intents : int;
  user_init : unit -> 'user_state;
  user_handler : 'user_state user_handler;
  ffmpeg_path : string;
  ffmpeg_options : string list;
  youtubedl_path : string;
}

type call_msg = |
type call_reply = |
type cast_msg = Event.t
type basic_msg = (call_msg, call_reply, cast_msg) Actaa.Gen_server.basic_msg
type msg = basic_msg

type 'user_state state = {
  agent : Agent.t;
  rest : Rest.t;
  user_state : 'user_state;
  user_handler : 'user_state user_handler;
}

class ['user_state] t =
  object (self)
    inherit
      ['user_state init_arg, msg, 'user_state state] Actaa.Gen_server.behaviour

    method private init env ~sw
        {
          token;
          intents;
          user_init;
          user_handler;
          ffmpeg_path;
          ffmpeg_options;
          youtubedl_path;
        } =
      let agent = new Agent.t in
      agent
      |> Actaa.Gen_server.start env ~sw
           Agent.
             {
               token;
               intents;
               consumer = (self :> consumer);
               ffmpeg_path;
               ffmpeg_options;
               youtubedl_path;
             };
      let rest = Rest.start env ~sw ~max_running:5 ~token in
      let user_state = user_init () in
      { agent; rest; user_state; user_handler }

    method! private handle_cast env ~sw state event =
      let user_state =
        try
          state.user_handler env ~sw state.agent state.rest state.user_state
            event
        with e ->
          Logs.err (fun m ->
              m "User consumer failed: %s\n%s" (Printexc.to_string e)
                (Printexc.get_backtrace ()));
          state.user_state
      in
      `NoReply { state with user_state }
  end

let default_ffmpeg_path = "/usr/bin/ffmpeg"

let default_ffmpeg_options =
  [
    "-i";
    "pipe:0";
    "-ac";
    "2";
    "-ar";
    "48000";
    "-f";
    "s16le";
    "-loglevel";
    "quiet";
    "pipe:1";
  ]

let default_youtubedl_path = "/usr/bin/youtube-dl"

let start env ~sw ~token ~intents ?(ffmpeg_path = default_ffmpeg_path)
    ?(ffmpeg_options = default_ffmpeg_options)
    ?(youtubedl_path = default_youtubedl_path) user_init user_handler =
  let t = new t in
  t
  |> Actaa.Gen_server.start env ~sw
       {
         token;
         intents;
         user_init;
         user_handler;
         ffmpeg_path;
         ffmpeg_options;
         youtubedl_path;
       };
  t
