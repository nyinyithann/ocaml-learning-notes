type 'a t = {o: 'a list; i: 'a list}

exception Empty

let create () = {o= []; i= []}

let is_empty = function {o; _} -> o = []

let length {o; i} = List.(length o + length i)

let enqueue x = function
  | {o= []; _} ->
      {o= [x]; i= []}
  | {o; i} ->
      {o; i= x :: i}

let dequeue = function
  | {o= []; _} ->
      raise Empty
  | {o= [_]; i} ->
      {o= List.rev i; i= []}
  | {o= _ :: t; i} ->
      {o= t; i}

let dequeue_opt = function
  | {o= []; _} ->
      None
  | {o= [_]; i} ->
      Some {o= List.rev i; i= []}
  | {o= _ :: t; i} ->
      Some {o= t; i}

let peek = function {o= []; _} -> raise Empty | {o= h :: _; _} -> h

let peek_opt = function {o= []; _} -> None | {o= h :: _; _} -> Some h

let iter ~f {o; i} =
  List.iter f o ;
  List.iter f (List.rev i)

let fold ~f acc {o; i} =
  let acc' = List.fold_left f acc o in
  List.fold_left f acc' (List.rev i)

let to_list {o; i} = o @ List.rev i

let of_list l = {o= l; i= []}

let of_seq seq = {o= List.of_seq seq; i= []}

let to_seq {o; i} = List.to_seq (o @ List.rev i)
