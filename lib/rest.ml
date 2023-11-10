let request ~meth ?body env ~token path =
  let headers =
    [
      ("authorization", "Bot " ^ token);
      ("user-agent", "DiscordBot (https://github.com/ushitora-anqou/yomer, 0.1)");
      ("content-Type", "application/json");
      ("accept", "*/*");
    ]
  in
  let url = "https://discord.com/api/v10" ^ path in
  Logs.info (fun m ->
      m "REST request: %s %s [%s]"
        (Cohttp.Code.string_of_method meth)
        url
        (body |> Option.fold ~none:"" ~some:Yojson.Safe.to_string));
  let body = body |> Option.map (fun x -> `Fixed (Yojson.Safe.to_string x)) in
  Eio.Switch.run @@ fun sw ->
  let resp = Httpx.Http.request ~meth ~headers ?body env ~sw url in
  let body = Httpx.Http.drain_resp_body resp in
  let body =
    try body |> Yojson.Safe.from_string |> Option.some with _ -> None
  in
  (Cohttp.Response.status (fst resp), body)

let request (env : Eio_unix.Stdenv.base) ~token name ~meth ?body url =
  match request env ~token ~meth ?body url with
  | code, Some body when Cohttp.Code.(code |> code_of_status |> is_success) -> (
      Logs.info (fun m -> m "%s: %s" name (Yojson.Safe.to_string body));
      try Ok body
      with exn ->
        Error (Printf.sprintf "%s: %s" name (Printexc.to_string exn)))
  | code, body ->
      Error
        (Printf.sprintf "%s: %s: %s" name
           (Cohttp.Code.string_of_status code)
           (body |> Option.fold ~none:"" ~some:Yojson.Safe.to_string))
  | exception exn ->
      Error (Printf.sprintf "%s failed: %s" name (Printexc.to_string exn))

module M = struct
  type msg = {
    name : string;
    meth : Cohttp.Code.meth;
    body : Yojson.Safe.t option;
    url : string;
  }

  type reply = (Yojson.Safe.t, string) result
  type param = { token : string }

  let process (env : Eio_unix.Stdenv.base) { token } { name; meth; body; url } :
      reply =
    request env ~token name ~meth ?body url
end

open Rate_limiter.Make (M)

type nonrec t = t

let start env ~sw ~max_running ~token = start env ~sw ~max_running M.{ token }

let request name ~meth ?body url (t : t) =
  rate_limited_process M.{ name; meth; body; url } t

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type create_message_param = {
  content : string option;
  embeds : Entity.embed list option; [@yojson.option]
}
[@@deriving make, yojson]

let create_message channel_id t p =
  request "create_message" ~meth:`POST
    ~body:(yojson_of_create_message_param p)
    ("/channels/" ^ channel_id ^ "/messages")
    t
  |> Result.map Entity.message_of_yojson

let get_user ~user_id t =
  request __FUNCTION__ ~meth:`GET ("/users/" ^ user_id) t
  |> Result.map Entity.user_of_yojson

let get_guild_member ~user_id ~guild_id t =
  request __FUNCTION__ ~meth:`GET
    ("/guilds/" ^ guild_id ^ "/members/" ^ user_id)
    t
  |> Result.map Entity.guild_member_of_yojson

let get_guild_roles ~guild_id t =
  request __FUNCTION__ ~meth:`GET ("/guilds/" ^ guild_id ^ "/roles") t
  |> Result.map (fun x ->
         x |> Yojson.Safe.Util.to_list |> List.map Entity.role_of_yojson)

let get_guild_channels ~guild_id t =
  request __FUNCTION__ ~meth:`GET ("/guilds/" ^ guild_id ^ "/channels") t
  |> Result.map (fun x ->
         x |> Yojson.Safe.Util.to_list |> List.map Entity.channel_of_yojson)

let get_channel ~channel_id t =
  request __FUNCTION__ ~meth:`GET ("/channels/" ^ channel_id) t
  |> Result.map Entity.channel_of_yojson
