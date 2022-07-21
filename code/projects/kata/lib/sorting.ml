let is_sorted l =
    let rec aux l cmp = 
    match l with
    | [] | [_] -> true
    | x :: ((y :: _) as t) -> cmp x y && aux t cmp
    in 
    aux l ( <= ) || aux l ( >= ) 


let%test _ = is_sorted [] = true
let%test _ = is_sorted [1; 2] = true
let%test _ = is_sorted [2; 1] = true
let%test _ = is_sorted [1; 2; 3] = true
let%test _ = is_sorted [1; 1; 1; 3] = true
let%test _ = is_sorted [3; 2; 1; 1] = true
let%test _ = is_sorted [1; 1; 1; 1; 1; 1; 0] = true
let%test _ = is_sorted [1; 1; 1; 3; 3; 3; 3; 4] = true
let%test _ = is_sorted [5; 2; 1; 0] = true
let%test _ = is_sorted [1; 1; 1; 1; 1] = true
let%test _ = is_sorted (List.init 10000 Fun.id) = true
let%test _ = is_sorted (List.init 10000 (fun x -> 10000 - x)) = true
let%test _ = is_sorted [1; 2; 0] = false

let%expect_test "is_sorted" =
    Printf.printf "%b" @@ is_sorted [1; 2; 0] ;
    [%expect {| false |}]
