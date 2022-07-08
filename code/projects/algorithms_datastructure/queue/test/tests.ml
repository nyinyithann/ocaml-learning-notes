let test_fn ~name =
  QCheck2.(
    Test.make ~name ~count:1_000 ~print:Print.(list float) Gen.(list float))
in

let is_empty_test _ = Queue.is_empty (Queue.create ()) in

let length_test l = Queue.of_list l |> Queue.length = List.length l in

let enqueue_test l =
  l
  |> List.fold_left (fun q x -> Queue.enqueue x q) (Queue.create ())
  |> Queue.to_list = l
in

let dequeue_test l =
  let q = Queue.of_list l in
  match l with
  | [] -> (
    try
      Queue.dequeue q |> ignore ;
      true
    with Queue.Empty -> true )
  | [_] ->
      Queue.dequeue q |> Queue.to_list = []
  | [_; _] ->
      q |> Queue.dequeue |> Queue.dequeue |> Queue.to_list = []
  | _ :: _ :: tl ->
      q |> Queue.dequeue |> Queue.dequeue |> Queue.to_list = tl
in

let dequeue_opt_test l =
  let q = Queue.of_list l in
  let ( let* ) = Option.bind in
  match l with
  | [] ->
      q |> Queue.dequeue_opt = None
  | [_] ->
      (let* x = q |> Queue.dequeue_opt in
       Some (x |> Queue.to_list) )
      = Some []
  | [_; _] ->
      (let* x = q |> Queue.dequeue_opt in
       let* y = x |> Queue.dequeue_opt in
       Some (y |> Queue.to_list) )
      = Some []
  | _ :: _ :: tl ->
      (let* x = q |> Queue.dequeue_opt in
       let* y = x |> Queue.dequeue_opt in
       Some (y |> Queue.to_list) )
      = Some tl
in

let peek_test l =
  let q = Queue.of_list l in
  match l with
  | [] -> (
    try
      q |> Queue.peek |> ignore ;
      true
    with Queue.Empty -> true )
  | h :: _ ->
      q |> Queue.peek = h
in

let peek_opt_test l =
  let q = Queue.of_list l in
  match l with
  | [] ->
      q |> Queue.peek_opt = None
  | h :: _ ->
      q |> Queue.peek_opt = Some h
in

let iter_test l =
  let q = Queue.of_list l in
  let i = ref 0 in
  let arr = Array.make (List.length l) 0. in
  Queue.iter
    ~f:(fun x ->
      arr.(!i) <- x +. x ;
      incr i )
    q ;
  Array.to_list arr = (l |> List.map (fun x -> x +. x))
in

let fold_test l =
  l |> Queue.of_list |> Queue.fold ~f:( +. ) 0. = List.fold_left ( +. ) 0. l
in

let seq_test l =
  let q = Queue.of_seq (List.to_seq l) in
  q |> Queue.to_seq |> List.of_seq = l
in

let queue_fns =
  [ ("is_empty", is_empty_test)
  ; ("length", length_test)
  ; ("enqueue", enqueue_test)
  ; ("dequeue", dequeue_test)
  ; ("dequeue_opt", dequeue_opt_test)
  ; ("peek_test", peek_test)
  ; ("peek_opt_test", peek_opt_test)
  ; ("iter_test", iter_test)
  ; ("fold_test", fold_test)
  ; ("seq_test", seq_test) ]
in

let tests = queue_fns |> List.map (fun (name, f) -> test_fn ~name f) in
QCheck_runner.run_tests ~colors:true ~verbose:true tests
