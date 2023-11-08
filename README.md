# Discord.ml

A [Discord](https://discord.com/) library purely written in OCaml, based on [Eio](https://github.com/ocaml-multicore/eio).

## Usage

Discord.ml has not yet been published to OPAM, so you need to pin this GitHub repository before using it:

```
opam pin https://github.com/ushitora-anqou/discordml.git#master
```

## Example

A music bot for YouTube can be written as follows
(see `example` directory for complete code):

```ocaml
type command = Ping | Join | Leave | Play of string (* youtube url *)

let parse_command s = (* ... snip ... *)

let handle_event token env ~sw:_ agent state =
  let open Discord.Event in
  function
  | Dispatch (MESSAGE_CREATE msg) -> (
      let guild_id = Option.get msg.guild_id in
      match parse_command msg.content with
      | Ok Ping ->
          Logs.info (fun m -> m "ping");
          if
            Discord.Rest.make_create_message_param
              ~embeds:[ Discord.Object.make_embed ~description:"pong" () ]
              ()
            |> Discord.Rest.create_message env ~token msg.channel_id
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
      | _ -> state)
  | _ -> state

let () =
  (* ... snip ... *)

  Eio_main.run @@ fun env ->
  Mirage_crypto_rng_eio.run (module Mirage_crypto_rng.Fortuna) env @@ fun () ->
  Eio.Switch.run @@ fun sw ->
  let _consumer : _ Discord.Consumer.t =
    Discord.Consumer.start env ~sw ~token ~intents
      (fun () -> ())
      (handle_event token)
  in
  ()
```

Text `!join` to join the bot to your voice channel and use `!play YOUTUBE-URL` to play your favourite music.

## Build locally

Set up [OPAM](https://opam.ocaml.org/) first. Then, run the following steps:

```
opam switch create --no-install . 5.0.0
opam install . --deps-only
dune build example/main.exe
```

## Note

Discord.ml was originally written for a Discord bot [Yomer](https://github.com/ushitora-anqou/yomer), which reads text messages aloud.
