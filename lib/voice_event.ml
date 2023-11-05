open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type ready = { ssrc : int; ip : string; port : int; modes : string list }
[@@yojson.allow_extra_fields] [@@deriving yojson, show]

type identify = {
  server_id : string;
  user_id : string;
  session_id : string;
  token : string;
}
[@@yojson.allow_extra_fields] [@@deriving yojson, show]

type select_protocol_data = { address : string; port : int; mode : string }
[@@yojson.allow_extra_fields] [@@deriving yojson, show]

type select_protocol = { protocol : string; data : select_protocol_data }
[@@yojson.allow_extra_fields] [@@deriving yojson, show]

type session_description = { mode : string; secret_key : int list }
[@@yojson.allow_extra_fields] [@@deriving yojson, show]

type speaking = {
  speaking : int;
  delay : int option; [@yojson.option]
  ssrc : int;
}
[@@yojson.allow_extra_fields] [@@deriving yojson, show]

type hello = { heartbeat_interval : float }
[@@yojson.allow_extra_fields] [@@deriving yojson, show]

type resume = { server_id : string; session_id : string; token : string }
[@@yojson.allow_extra_fields] [@@deriving yojson, show]

type t =
  | Identify (* 0 *) of identify
  | SelectProtocol (* 1 *) of select_protocol
  | Ready (* 2 *) of ready
  | Heartbeat (* 3 *) of int (* nonce *)
  | SessionDescription (* 4 *) of session_description
  | Speaking (* 5 *) of speaking
  | HeartbeatAck (* 6 *) of int (* nonce *)
  | Resume (* 7 *) of resume
  | Hello (* 8 *) of hello
  | Resumed (* 9 *)
  | Unknown_13 (* 13 *)
  | Unknown_18 (* 18 *)
  | Unknown_20 (* 20 *)

let of_yojson json =
  let open Yojson.Safe.Util in
  try
    let op = json |> member "op" |> to_int in
    let d = json |> member "d" in
    match op with
    | 0 -> d |> identify_of_yojson |> fun x -> Identify x
    | 1 -> d |> select_protocol_of_yojson |> fun x -> SelectProtocol x
    | 2 -> d |> ready_of_yojson |> fun x -> Ready x
    | 3 -> d |> to_int |> fun x -> Heartbeat x
    | 4 -> d |> session_description_of_yojson |> fun x -> SessionDescription x
    | 5 -> d |> speaking_of_yojson |> fun x -> Speaking x
    | 6 -> d |> to_int |> fun x -> HeartbeatAck x
    | 7 -> d |> resume_of_yojson |> fun x -> Resume x
    | 8 -> d |> hello_of_yojson |> fun x -> Hello x
    | 9 -> Resumed
    | 13 -> Unknown_13
    | 18 -> Unknown_18
    | 20 -> Unknown_20
    | _ -> failwith "Invalid op"
  with Type_error _ -> failwith "Invalid JSON for event"

let to_yojson = function
  | Identify d -> `Assoc [ ("op", `Int 0); ("d", yojson_of_identify d) ]
  | SelectProtocol d ->
      `Assoc [ ("op", `Int 1); ("d", yojson_of_select_protocol d) ]
  | Ready d -> `Assoc [ ("op", `Int 2); ("d", yojson_of_ready d) ]
  | Heartbeat d -> `Assoc [ ("op", `Int 3); ("d", `Int d) ]
  | SessionDescription d ->
      `Assoc [ ("op", `Int 4); ("d", yojson_of_session_description d) ]
  | Speaking d -> `Assoc [ ("op", `Int 5); ("d", yojson_of_speaking d) ]
  | HeartbeatAck d -> `Assoc [ ("op", `Int 6); ("d", `Int d) ]
  | Resume d -> `Assoc [ ("op", `Int 7); ("d", yojson_of_resume d) ]
  | Hello d -> `Assoc [ ("op", `Int 8); ("d", yojson_of_hello d) ]
  | Resumed -> `Assoc [ ("op", `Int 9); ("d", `Null) ]
  | Unknown_13 | Unknown_18 | Unknown_20 -> assert false
