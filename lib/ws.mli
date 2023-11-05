type conn

val connect :
  ?extra_headers:Cohttp.Header.t ->
  sw:Eio.Switch.t ->
  Eio_unix.Stdenv.base ->
  string ->
  conn

val id : conn -> string
val read : conn -> Websocket.Frame.t
val write : conn -> Websocket.Frame.t -> unit

module Process : sig
  type msg =
    [ `WSText of string * conn
    | `WSClose of [ `Status_code of int | `Unknown ] * conn ]

  val start : sw:Eio.Switch.t -> conn -> [> msg ] Actaa.Process.t1 -> unit
end
