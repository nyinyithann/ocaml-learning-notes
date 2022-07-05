type 'a t = Nil | Cons of 'a * 'a t

exception Empty

let create () = Nil

let is_empty = function Nil -> true | _ -> false

let length s =
  let rec loop s len =
    match s with Nil -> len | Cons (_, xs) -> loop xs (len + 1)
  in
  loop s 0

let push x s = Cons (x, s)

let pop = function Nil -> raise Empty | Cons (_, xs) -> xs

let pop_opt = function Nil -> None | Cons (_, xs) -> Some xs

let peek = function Nil -> raise Empty | Cons (h, _) -> h

let peek_opt = function Nil -> None | Cons (h, _) -> Some h

let rec iter ~f s = match s with Nil -> () | Cons (h, t) -> f h ; iter ~f t

let rec fold ~f acc s =
  match s with Nil -> acc | Cons (h, t) -> fold ~f (f acc h) t

let to_list s =
  let rec loop s l =
    match s with Nil -> l | Cons (h, xs) -> loop xs (h :: l)
  in
  loop s []

let to_seq s =
  let rec loop s () =
    match s with Nil -> Seq.Nil | Cons (h, xs) -> Seq.Cons (h, loop xs)
  in
  loop s

let of_list l =
  let rec loop l s = match l with [] -> s | h :: t -> loop t (Cons (h, s)) in
  loop l Nil

let of_seq seq =
  let rec loop seq s =
    match seq () with
    | Seq.Nil ->
        s
    | Seq.Cons (h, rest) ->
        loop rest (Cons (h, s))
  in
  loop seq Nil
