let test_fn ~name =
  QCheck2.(
    Test.make ~name ~count:1_000 ~print:Print.(list float) Gen.(list float))
in

let create_stack l = Stack.of_list l in

let create_test _ = Stack.create () |> Stack.is_empty in

let is_empty_test _ = create_stack [] |> Stack.is_empty in

let push_peek_test l =
  let st = List.fold_left (fun s x -> s |> Stack.push x) (Stack.create ()) l in
  st = create_stack l
  && Stack.push 101. st |> Stack.peek = 101.
  && Stack.push 111. st |> Stack.pop |> Stack.to_list = l
in

let of_list_to_list_test l = Stack.to_list (Stack.of_list l) = l in

let of_seq_to_seq_test l =
  l |> List.to_seq |> Stack.of_seq |> Stack.to_seq |> List.of_seq = List.rev l
in

let pop_test l =
  let st = create_stack l in
  let revl = List.rev l in
  match revl with
  | [] -> (
    try
      ignore (Stack.pop st) ;
      true
    with Stack.Empty -> true )
  | _ :: t ->
      Stack.pop st |> Stack.to_list = List.rev t
in

let iter_test l =
  let arr = Array.create_float (List.length l) in
  let st = create_stack l in
  let i = ref 0 in
  st |> Stack.iter ~f:(fun x -> arr.(!i) <- x ; incr i) ;
  Array.to_list arr = List.rev l
in

let fold_test l =
  let st = create_stack l in
  st |> Stack.fold ~f:(fun acc x -> x :: acc) [] = l
in

let stack_fns =
  [ ("create", create_test)
  ; ("is_empty", is_empty_test)
  ; ("push/peek", push_peek_test)
  ; ("of_list/to_list", of_list_to_list_test)
  ; ("of_seq/to_seq", of_seq_to_seq_test)
  ; ("pop", pop_test)
  ; ("iter", iter_test)
  ; ("fold", fold_test) ]
in
let tests = stack_fns |> List.map (fun (name, f) -> test_fn ~name f) in
QCheck_runner.run_tests ~colors:true ~verbose:true tests
