let handle_event token env ~sw:_ agent state =
  let open Discord.Event in
  function
  | Dispatch (MESSAGE_CREATE msg) -> (
      let guild_id = Option.get msg.guild_id in
      let parsed = String.split_on_char ' ' msg.content in
      match parsed with
      | [ "!ping" ] ->
          Logs.info (fun m -> m "ping");
          if
            Discord.Rest.make_create_message_param
              ~embeds:[ Discord.Object.make_embed ~description:"pong" () ]
              ()
            |> Discord.Rest.create_message env ~token msg.channel_id
            |> Result.is_error
          then Logs.err (fun m -> m "Failed to send pong");
          state
      | [ "!join" ] ->
          (match
             agent
             |> Discord.Agent.get_voice_states ~guild_id ~user_id:msg.author.id
           with
          | None -> ()
          | Some vstate -> (
              match vstate.Discord.Event.channel_id with
              | None -> ()
              | Some channel_id ->
                  agent |> Discord.Agent.join_channel ~guild_id ~channel_id));
          state
      | [ "!leave" ] ->
          agent |> Discord.Agent.leave_channel ~guild_id;
          state
      | [ "!play"; url ] ->
          Logs.info (fun m -> m "Playing %s" url);
          agent |> Discord.Agent.play_voice ~guild_id ~src:(`Ytdl url);
          state
      | _ -> state)
  | _ -> state

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);

  let token =
    match Sys.getenv_opt "DISCORD_TOKEN" with
    | Some s -> s
    | None -> failwith "DISCORD_TOKEN not set"
  in
  let intents =
    Discord.Intent.encode
      [ GUILDS; GUILD_VOICE_STATES; GUILD_MESSAGES; MESSAGE_CONTENT ]
  in
  let youtubedl_path = Sys.getenv_opt "YOUTUBEDL_PATH" in
  let ffmpeg_path = Sys.getenv_opt "FFMPEG_PATH" in

  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let _consumer : _ Discord.Consumer.t =
    Discord.Consumer.start env ~sw ~token ~intents ?ffmpeg_path ?youtubedl_path
      (fun () -> ())
      (handle_event token)
  in
  ()
