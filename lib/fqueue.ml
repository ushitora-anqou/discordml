exception Empty

module Q = struct
  type 'a t = 'a list * 'a list

  let empty = ([], [])
  let is_empty (f, _) = f = []
  let check_f = function [], r -> (List.rev r, []) | q -> q
  let snoc (f, r) x = check_f (f, x :: r)
  let head = function [], _ -> raise Empty | x :: _, _ -> x
  let tail = function [], _ -> raise Empty | _ :: f, r -> check_f (f, r)
end

type 'a t = 'a Q.t

let empty = Q.empty
let add = Q.snoc
let push = Q.snoc
let take q = (Q.head q, Q.tail q)
let take_opt q = try (Some (Q.head q), Q.tail q) with Empty -> (None, q)
let pop = take
let peek = Q.head
let peek_opt q = try Some (Q.head q) with Empty -> None
let top = peek
let is_empty = Q.is_empty
