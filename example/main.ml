type command =
  | Ping
  | Join
  | Leave
  | Play of string (* youtube url *)
  | Say of string (* message *)
  | Chat of string (* message *)

let parse_command s =
  let parsed = String.split_on_char ' ' s in
  match parsed with
  | [ "!ping" ] -> Ok Ping
  | [ "!join" ] -> Ok Join
  | [ "!leave" ] -> Ok Leave
  | [ "!play"; url ] -> Ok (Play url)
  | "!say" :: tl -> Ok (Say (tl |> String.concat " "))
  | "!chat" :: tl -> Ok (Chat (tl |> String.concat " "))
  | _ -> Error "Invalid command"

let handle_event ?openai _env ~sw:_ agent rest state =
  let open Discord.Event in
  function
  | Dispatch (MESSAGE_CREATE msg) -> (
      let guild_id = Option.get msg.guild_id in
      match parse_command msg.content with
      | Ok Ping ->
          Logs.info (fun m -> m "ping");
          if
            Discord.Rest.make_create_message_param
              ~embeds:[ Discord.Entity.make_embed ~description:"pong" () ]
              ()
            |> Discord.Rest.create_message msg.channel_id rest
            |> Result.is_error
          then Logs.err (fun m -> m "Failed to send pong");
          state
      | Ok Join ->
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
      | Ok Leave ->
          agent |> Discord.Agent.leave_channel ~guild_id;
          state
      | Ok (Play url) ->
          Logs.info (fun m -> m "Playing %s" url);
          agent |> Discord.Agent.play_voice ~guild_id ~src:(`Ytdl url);
          state
      | Ok (Say text) ->
          Logs.info (fun m -> m "Say text");
          if
            Discord.Rest.make_create_message_param ~content:text ()
            |> Discord.Rest.create_message msg.channel_id rest
            |> Result.is_error
          then Logs.err (fun m -> m "Failed to send say response");
          state
      | Ok (Chat text) ->
          Logs.info (fun m -> m "Chat %s" text);
          (match openai with
          | None -> Logs.err (fun m -> m "OpenAI not set properly.")
          | Some openai ->
              let model = "gpt-3.5-turbo-1106" in
              let bot_name = "discordml" in
              let temperature = 0.7 in
              let response_max_tokens = 99 in
              let persona =
                Printf.sprintf
                  "As '%s', you're a character with a enigmatic aura yet a \
                   humorous spark, reminiscent of a witty person. Your Korean \
                   responses should be under %d tokens."
                  bot_name response_max_tokens
              in
              let max_tokens =
                let prompt_tokens =
                  let chars_per_token = 4 in
                  String.length persona / chars_per_token
                in
                prompt_tokens + response_max_tokens
              in
              let answer_r =
                Discord.Openai.make_create_message_param ~model
                  ~messages:
                    [
                      Discord.Openai.make_user_message ~role:"system"
                        ~content:persona ();
                      Discord.Openai.make_user_message ~content:text ();
                    ]
                  ~max_tokens ~temperature ()
                |> Discord.Openai.create_message openai
              in
              if
                Result.bind answer_r (fun answer ->
                    Discord.Rest.make_create_message_param ~content:answer ()
                    |> Discord.Rest.create_message msg.channel_id rest)
                |> Result.is_error
              then Logs.err (fun m -> m "Failed to send chat response"));
          state
      | _ -> state)
  | _ -> state

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Logs.set_level (Some Logs.Info);
  let token = Sys.getenv "DISCORD_TOKEN" in
  let youtubedl_path = Sys.getenv_opt "YOUTUBEDL_PATH" in
  let ffmpeg_path = Sys.getenv_opt "FFMPEG_PATH" in
  let intents =
    Discord.Intent.encode
      [ GUILDS; GUILD_VOICE_STATES; GUILD_MESSAGES; MESSAGE_CONTENT ]
  in
  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let open Discord.Openai in
  let openai =
    let openai_token = Sys.getenv_opt "OPENAI_TOKEN" in
    openai_token
    |> Option.map (fun openai_token ->
           start env ~sw ~max_running:5 ~openai_token)
  in
  let _consumer : _ Discord.Consumer.t =
    Discord.Consumer.start env ~sw ~token ~intents ?ffmpeg_path ?youtubedl_path
      (fun () -> ())
      (handle_event ?openai)
  in
  ()
