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
let ( <**> ) = both

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
  let+ f
  and+ p in
  f p
;;

let ( <*> ) = apply
let lift = ( >>| )
let lift2 f p1 p2 = return f <*> p1 <*> p2

let rec all parsers =
  let cons h t = h :: t in
  let consP = lift2 cons in
  match parsers with
  | [] -> return []
  | h :: t -> consP h (all t)
;;

let rec many p =
  (let* x = p in
   let* xs = many p in
   return (x :: xs))
  <|> return []
;;

let many1 p =
  let* x = p in
  let* xs = many p in
  return (x :: xs)
;;

let between p1 p2 p3 = p1 *> p2 <* p3

let opt p =
  let some = p >>| Option.some in
  let none = return None in
  some <|> none
;;

let sepby1 p sep =
  let sep_and_p = sep *> p in
  p <**> many sep_and_p >>| fun (p, plist) -> p :: plist
;;

let sepby p sep = sepby1 p sep <|> return []

module P = struct
  (*  let explode s =  *)
  (*      let rec aux i l = if i < 0 then l else aux (i - 1) (s.[i] :: l)  *)
  (*      in aux (String.length s - 1) [] in *)
  (*  *)
  (* let implode l =  *)
  (*      let r = Bytes.create (List.length l) in *)
  (*      let rec aux i = function [] -> r | h :: t -> Bytes.set r i h ; aux (i + 1) l  *)
  (*      in (aux 0 l) |> Bytes.unsafe_to_string in *)

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
  let digit_parser = List.init 10 (fun x -> char_of_int (48 + x)) |> any_of
  let letter_parser = List.init 26 (fun x -> char_of_int (97 + x)) |> any_of

  let parse_3digits_as_int =
    let open Printf in
    let+ (a, b), c = digit_parser <**> digit_parser <**> digit_parser in
    int_of_string @@ sprintf "%c%c%c" a b c
  ;;

  let pa = char_parser 'a'
  let pb = char_parser 'b'
  let pc = char_parser 'c'
  let implode chars = chars |> List.to_seq |> String.of_seq
  let explode s = s |> String.to_seq |> List.of_seq
  let int_of_char_list chars = int_of_string @@ implode chars
  let integer_of_char ch = int_of_string @@ Printf.sprintf "%c" ch

  let pstring str =
    str |> String.to_seq |> Seq.map char_parser |> List.of_seq |> all >>| implode
  ;;

  let int_parser =
    let digits = many1 digit_parser in
    digits >>| int_of_char_list
  ;;

  let to_int (copt, cl) =
    let i = int_of_char_list cl in
    match copt with
    | Some _ -> -i
    | None -> i
  ;;

  let int_with_sign_parser =
    let digits = many1 digit_parser in
    let p = opt (char_parser '-') <**> digits in
    p >>| to_int
  ;;

  let integer_with_semicolon =
    let digit = many1 digit_parser in
    digit <* char_parser ';' >>| int_of_char_list
  ;;

  let let_p = char_parser 'l' <**> char_parser 'e' <**> char_parser 't'
  let whitespace_p = any_of [ ' '; '\t'; '\n'; '\r' ]
  let one_or_more_whitespace_p = many1 whitespace_p
  let one_or_more_letter_p = many1 letter_parser
  let equal_p = char_parser '='
  let one_or_more_digit_p = many1 digit_parser

  let let_binding_int_parser =
    let_p
    <* one_or_more_whitespace_p
    <**> one_or_more_letter_p
    <* one_or_more_whitespace_p
    <**> equal_p
    <* one_or_more_whitespace_p
    <**> one_or_more_digit_p
    <* one_or_more_whitespace_p
  ;;
end

(* open P *)
