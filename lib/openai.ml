let request ~meth ?body env ~openai_token path =
  let headers =
    [
      ("authorization", "Bearer " ^ openai_token);
      ("user-agent", "DiscordBot (https://github.com/ushitora-anqou/yomer, 0.1)");
      ("content-Type", "application/json");
      ("accept", "*/*");
    ]
  in
  let url = "https://api.openai.com/v1" ^ path in
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

let request (env : Eio_unix.Stdenv.base) ~openai_token name ~meth ?body url =
  match request env ~openai_token ~meth ?body url with
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

  (* type param = { openai_token : string; model : string; max_tokens : int } *)
  type param = { openai_token : string }

  let process (env : Eio_unix.Stdenv.base) { openai_token; _ }
      { name; meth; body; url } : reply =
    request env ~openai_token name ~meth ?body url
end

open Rate_limiter.Make (M)

type nonrec t = t

let start env ~sw ~max_running ~openai_token =
  start env ~sw ~max_running M.{ openai_token }

let request name ~meth ?body url (t : t) =
  rate_limited_process M.{ name; meth; body; url } t

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type user_message = {
  role : string; [@default "user"]
  content : string option;
  name : string option; [@yojson.option]
}
[@@deriving yojson, show, make]

(*  https://platform.openai.com/docs/api-reference/chat/create*)
type create_message_param = {
  messages : user_message list; (* TODO Implement all possible variations*)
  model : string; [@default "gpt-3.5-turbo"]
  frequency_penalty : float option; [@yojson.option]
  (* logit_bias: string option; *)
  logprobs : bool option; [@yojson.option]
  top_logprobs : int option; [@yojson.option]
  max_tokens : int; [@default 250]
  n : int option; [@yojson.option]
  presence_penalty : float option; [@yojson.option]
  (* response_format *)
  seed : int option; [@yojson.option]
  (* stop *)
  stream : bool option; [@yojson.option]
  temperature : float option; [@yojson.option]
  top_p : float option; [@yojson.option]
  (* tools *)
  (* tool_choices *)
  user : string option; [@yojson.option]
}
[@@deriving make, yojson]

let create_message t p =
  let open Yojson.Safe.Util in
  request "create_message" ~meth:`POST
    ~body:(yojson_of_create_message_param p)
    "/chat/completions" t
  |> Result.map (fun r ->
         r |> member "choices" |> to_list
         |> List.map (fun choice ->
                choice |> member "message" |> member "content" |> to_string)
         |> String.concat "\n")
