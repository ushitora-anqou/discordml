module StringMap = Map.Make (String)

module With_mutex = struct
  type 'a t = { mtx : Eio.Mutex.t; v : 'a }

  let make v = { mtx = Eio.Mutex.create (); v }
  let ro_lock { mtx; v } f = Eio.Mutex.use_ro mtx (fun () -> f v)
  let rw_lock { mtx; v } f = Eio.Mutex.use_rw mtx (fun () -> f v)
end

type json_any = Yojson.Safe.t [@@deriving show]

let json_any_of_yojson = Fun.id
let yojson_of_json_any = Fun.id

module List = struct
  include List

  let take_at_most n =
    let rec aux n acc = function
      | [] -> (List.rev acc, [])
      | rest when n = 0 -> (List.rev acc, rest)
      | x :: xs -> aux (n - 1) (x :: acc) xs
    in
    aux n []
end

let now () = Ptime.to_float_s (Ptime.v (Pclock.now_d_ps ()))

module Option = struct
  include Option

  let flatten = function Some x -> x | None -> None
end

module Result = struct
  include Result

  let ( >>= ) = bind
end

let iota n = List.init n Fun.id
