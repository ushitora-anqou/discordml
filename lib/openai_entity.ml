[@@@warning "-30"]

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type top_logprob = {
  token : string;
  logprob : float;
  bytes : int list option; [@yojson.option]
}
[@@yojson.allow_extra_fields] [@@deriving yojson, show, make]

type function' = { name : string; arguments : string }
[@@yojson.allow_extra_fields] [@@deriving yojson, show, make]

type tool_call = {
  id : string;
  type' : string; [@key "type"]
  function' : function'; [@key "function"]
}
[@@yojson.allow_extra_fields] [@@deriving yojson, show, make]

type message' = {
  content : string option;
  tool_calls : tool_call list option; [@yojson.option]
  role : string;
}
[@@yojson.allow_extra_fields] [@@deriving yojson, show, make]

type content = {
  token : string;
  logprob : float;
  bytes : int list option; [@yojson.option]
  top_logprobs : top_logprob list;
}
[@@yojson.allow_extra_fields] [@@deriving yojson, show, make]

type logprobs = { content : content list option }
[@@yojson.allow_extra_fields] [@@deriving yojson, show, make]

type choice = {
  finish_reason : string;
  index : int;
  message : message';
  logprobs : logprobs option;
}
[@@yojson.allow_extra_fields] [@@deriving yojson, show, make]

type usage = {
  completion_tokens : int;
  prompt_tokens : int;
  total_tokens : int;
}
[@@yojson.allow_extra_fields] [@@deriving yojson, show, make]

(* The chat completion object
   https://platform.openai.com/docs/api-reference/chat/object *)

type message = {
  id : string;
  choices : choice list;
  created : int;
  model : string;
  system_fingerprint : string;
  object_ : string; [@key "object"]
  usage : usage;
}
[@@yojson.allow_extra_fields] [@@deriving yojson, show, make]
