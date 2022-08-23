open Core

module type Comparable = sig
  type t

  val compare : t -> t -> int
end

(* a little more efficient *)
let is_sorted_1 (type a) (module M : Comparable with type t = a) (l : a list) =
  let rec aux l cmp =
    match l with
    | [] | [ _ ] | [ _; _ ] -> true
    | x :: (y :: _ as t) -> cmp x y && aux t cmp
  in
  let ( <= ) x y =
    let r = M.compare x y in
    r = -1 || r = 0
  and ( >= ) x y =
    let r = M.compare x y in
    r = 1 || r = 0
  in
  aux l ( <= ) || aux l ( >= )
;;

let is_sorted_2 (type a) (module M : Comparable with type t = a) (l : a list) =
  let rec aux l cmp =
    match l with
    | [] | [ _ ] | [ _; _ ] -> true
    | x :: y :: t -> cmp x y && aux (y :: t) cmp
  in
  let ( <= ) x y =
    let r = M.compare x y in
    r = -1 || r = 0
  and ( >= ) x y =
    let r = M.compare x y in
    r = 1 || r = 0
  in
  aux l ( <= ) || aux l ( >= )
;;

(* 3x efficient than explode_2 *)
let explode_1 s =
  let rec loop n l = if n < 0 then l else loop (n - 1) (s.[n] :: l) in
  loop (String.length s - 1) []
;;

let explode_2 s = s |> Caml.String.to_seq |> Caml.List.of_seq

(* same as explode_1*)
let explode_3 s = s |> String.to_list

open Core_bench

let main () =
  let ints = List.init 1_0 ~f:Fun.id
  and floats = List.init 1_0 ~f:float in
  let ints_bench_1 =
    Bench.Test.create ~name:"is_sorted_1 int list" (fun () ->
        is_sorted_1 (module Int) ints)
  and ints_bench_2 =
    Bench.Test.create ~name:"is_sorted_2 int list" (fun () ->
        is_sorted_2 (module Int) ints)
  and floats_bench_1 =
    Bench.Test.create ~name:"is_sorted_1 float list" (fun () ->
        is_sorted_1 (module Float) floats)
  and floats_bench_2 =
    Bench.Test.create ~name:"is_sorted_2 float list" (fun () ->
        is_sorted_2 (module Float) floats)
  in
  [ ints_bench_1; ints_bench_2; floats_bench_1; floats_bench_2 ] |> Bench.bench;
  let str = String.make 100000 'a' in
  let b1 = Bench.Test.create ~name:"explode recursive" (fun () -> explode_1 str)
  and b2 = Bench.Test.create ~name:"explode with Seq, List" (fun () -> explode_2 str)
  and b3 = Bench.Test.create ~name:"explode with Seq, List" (fun () -> explode_3 str) in
  [ b1; b2; b3 ] |> Bench.bench
;;

let _ = main ()
