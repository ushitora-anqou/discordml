module Make (M : sig
  type msg
  type reply
  type param

  val process : Eio_unix.Stdenv.base -> param -> msg -> reply
end) =
struct
  type init_arg = { max_running : int; user_param : M.param }
  type call_msg = [ `General of M.msg ]
  type call_reply = [ `General of M.reply ]
  type cast_msg = [ `Result of call_reply Actaa.Gen_server.from_ty * M.reply ]
  type basic_msg = (call_msg, call_reply, cast_msg) Actaa.Gen_server.basic_msg
  type msg = basic_msg

  type state = {
    queue : (call_reply Actaa.Gen_server.from_ty * M.msg) Fqueue.t;
    running : int;
    max_running : int;
    user_param : M.param;
  }

  class processor from param msg target =
    object
      inherit [unit, unit] Actaa.Process.t

      method on_spawn env ~sw:_ () =
        let reply = M.process env param msg in
        Actaa.Gen_server.cast target (`Result (from, reply));
        Actaa.Process.Stop_reason.Normal
    end

  let start_process env ~sw from param msg self =
    Actaa.Process.spawn env ~sw () (new processor from param msg self)

  class t =
    object (self)
      inherit [init_arg, msg, state] Actaa.Gen_server.behaviour

      method private init _env ~sw:_ { max_running; user_param } =
        { queue = Fqueue.empty; running = 0; max_running; user_param }

      method! private handle_call env ~sw from state =
        function
        | `General msg ->
            if state.running = state.max_running then
              `NoReply { state with queue = Fqueue.add state.queue (from, msg) }
            else (
              start_process env ~sw from state.user_param msg self;
              `NoReply { state with running = state.running + 1 })

      method! private handle_cast env ~sw state =
        function
        | `Result (from, reply_msg) -> (
            Actaa.Gen_server.reply from (`General reply_msg);
            match Fqueue.take_opt state.queue with
            | None, _ -> `NoReply { state with running = state.running - 1 }
            | Some (from, msg), queue ->
                start_process env ~sw from state.user_param msg self;
                `NoReply { state with queue })
    end

  let start env ~sw ~max_running user_param =
    let t = new t in
    Actaa.Gen_server.start env ~sw { max_running; user_param } t;
    t

  let rate_limited_process msg t =
    match Actaa.Gen_server.call t (`General msg) with `General reply -> reply
end
