type 'a parser_result =
  | Success of 'a * string
  | Failure of string

type 'a parser = string -> 'a parser_result

let return (x : 'a) : 'a parser =
  let p stream = Success (x, stream) in
  p
;;

let bind (p : 'a parser) (f : 'a -> 'b parser) : 'b parser =
  let q stream =
    match p stream with
    | Success (x, rest) -> (f x) rest
    | Failure _ as f -> f
  in
  q
;;

let ( let* ) = bind

let both (p1 : 'a parser) (p2 : 'b parser) : ('a * 'b) parser =
  let* x = p1 in
  let* y = p2 in
  return (x, y)
;;

let ( and+ ) = both
let ( <..> ) = both

let either (p1 : 'a parser) (p2 : 'a parser) : 'a parser =
  let q stream =
    match p1 stream with
    | Failure _ -> p2 stream
    | Success _ as s -> s
  in
  q
;;

let ( <|> ) = either

let choice parsers =
  if List.length parsers = 0
  then failwith "parser list is empty"
  else parsers |> List.fold_left (fun acc x -> acc <|> x) (List.hd parsers)
;;

let ( *> ) p1 p2 =
  let* _ = p1 in
  p2
;;

let ( <* ) p1 p2 =
  let* x = p1 in
  let* _ = p2 in
  return x
;;

let map (f : 'a -> 'b) (p : 'a parser) : 'b parser =
  let* x = p in
  return (f x)
;;

let ( let+ ) x f = map f x
let ( >>| ) x f = map f x

let apply (f : ('a -> 'b) parser) (p : 'a parser) : 'b parser =
    let+ f and+ p in f p    
;;

let ( <*> ) = apply

let lift = ( >>| )
let lift2 f p1 p2 =  return f <*> p1 <*> p2 

let rec sequence parsers =
    let cons h t = h :: t in 
    let consP = lift2 cons in 
    match parsers with
    | [] -> return [] 
    | h :: t -> consP h (sequence t)

module P = struct

  let char_parser ch : char parser =
    let p stream =
      let open Printf in
      try
        let first = String.get stream 0 in
        if ch = first
        then Success (ch, String.sub stream 1 (String.length stream - 1))
        else Failure (sprintf "Expected %c but found %c" ch first)
      with
      | Invalid_argument s -> Failure s
    in
    p
  ;;

  let any_of chars = chars |> List.map char_parser |> choice

  let digit_parser = 
        List.init 10 (fun x -> char_of_int (48 + x) ) |> any_of

  let parse_3digits_as_int = 
      let open Printf in
      let+ ((a,b),c) = digit_parser <..> digit_parser <..> digit_parser in 
      int_of_string @@ sprintf "%c%c%c" a b c

    let pa = char_parser 'a' 
    let pb = char_parser 'b'
    let pc = char_parser 'c'
end
