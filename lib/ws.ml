include Httpx.Ws

module Process = struct
  type msg =
    [ `WSText of string * conn
    | `WSClose of [ `Status_code of int | `Unknown ] * conn ]

  let start ~sw conn (caster : [> msg ] Actaa.Process.t1) =
    Eio.Fiber.fork ~sw @@ fun () ->
    let rec loop () =
      try
        let frame = read conn in
        Logs.info (fun m -> m "Ws received: %a" Websocket.Frame.pp frame);
        match frame.opcode with
        | Text ->
            (try Actaa.Process.send caster (`WSText (frame.content, conn))
             with e ->
               Logs.err (fun m ->
                   m "Ws handling event failed: %s: %s" (Printexc.to_string e)
                     frame.content));
            loop ()
        | Close ->
            let status_code = String.get_int16_be frame.content 0 in
            Logs.warn (fun m ->
                m "Websocket connection closed: code %d" status_code);
            Actaa.Process.send caster
              (`WSClose (`Status_code status_code, conn))
        | _ ->
            Logs.info (fun m ->
                m "Received non-text ws frame: %a" Websocket.Frame.pp frame);
            loop ()
      with e ->
        Logs.err (fun m ->
            m "Ws receiving failed: %s: %s" (Printexc.to_string e)
              (Printexc.get_backtrace ()));
        Actaa.Process.send caster (`WSClose (`Unknown, conn))
    in
    loop ()
end
