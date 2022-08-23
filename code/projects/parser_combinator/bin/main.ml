module Parser = struct
  type 'a parser_result = Success of 'a * string | Failure
  type 'a parser = string -> 'a parser_result
  let return a : 'a parser = let q s = Success (a, s) in q
  let bind p f = let q s = match p s with Success (a, r) -> (f a) r | Failure -> Failure in q
  let ( let* ) = bind
  let map f p = let* a = p in return (f a)
  let ( let+ ) x f = map f x
  let ( |>> ) x f = map f x
  let both p1 p2 = let* a = p1 in let* b = p2 in return (a, b)
  let ( and+ ) = both
  let ( <**> ) = both
  let apply f p = let+ f and+ p in f p
  let ( <~> ) = apply
  let ( *> ) p1 p2 = p1 <**> p2 |>> fun (_, b) -> b
  let ( <* ) p1 p2 = p1 <**> p2 |>> fun (a, b) -> a
  let either p1 p2 = let q s = match p1 s with | Failure -> p2 s | Success _ as y -> y in q
  let ( <|> ) = either
  let lift2 f p1 p2 = return f <~> p1 <~> p2
  let lift3 f p1 p2 p3 = return f <~> p1 <~> p2 <~> p3
  let rec sequence ps = match ps with | [] -> return [] | h :: t -> (lift2 List.cons) h (sequence t)
  let rec many p = (let* x = p in let* xs = many p in return (x :: xs)) <|> return []
  let many1 p = let* x = p in let* xs = many p in return (x :: xs)
  let between p1 p2 p3 = p1 *> p2 <* p3
  let opt p = p |>> Option.some <|> return None
  let sepby1 p sep = p <**> many (sep *> p) |>> fun (x, xs) -> x :: xs
  let sepby p sep = sepby1 p sep <|> return []
  let choice ps = ps |> List.fold_left (fun a p -> a <|> p) (List.hd ps)
  
  let explode s = s |> String.to_seq |> List.of_seq 
  let implode l = l |> List.to_seq |> String.of_seq 
  let char_parser c =
    let p s = match explode s with x :: xs when x = c -> Success (x, implode xs) | _ -> Failure in p
  let digit_parser =
    let cps = List.init 10 (fun x -> Char.chr (48 + x)) |> List.map char_parser in
    List.fold_left either (List.hd cps) cps
end

(*(*  parsing float num: /[+-]?(\d+(\.\d*)?|\d*\.\d+)([eE][+-]?\d+)?/  *)
let parse_float =
  let open Parser in
  let explode s = s |> String.to_seq |> List.of_seq in
  let implode l = l |> List.to_seq |> String.of_seq in
  let char_parser c =
    let p s = match explode s with x :: xs when x = c -> Success (x, implode xs) | _ -> Failure in p
  in
  let digit_parser =
    let cps = List.init 10 (fun x -> Char.chr (48 + x)) |> List.map char_parser in
    List.fold_left either (List.hd cps) cps
  in
    let one_or_more_digit = many1 digit_parser in
    let dot = char_parser '.' in
    let zero_or_more_digit = many digit_parser in
    let* s = opt (char_parser '+' <|> char_parser '-') in
    let* l =
      (let* l = one_or_more_digit in
       let* pd = (let* p = dot in let* d = zero_or_more_digit in return (p :: d)) <|> return []
       in return (l @ pd))
      <|> 
      let* l = zero_or_more_digit in let* p = dot in 
      let* d = one_or_more_digit in return (l @ (p :: d))
    in
    let* e =
      (let* e = char_parser 'e' <|> char_parser 'E' in
       let* s = opt (char_parser '+' <|> char_parser '-') in
       let* x = one_or_more_digit in return (Option.(if is_none s then e :: x else e :: get s :: x))) 
       <|> return []
    in
    return (Option.(if is_none s then l @ e else get s :: (l @ e)) |> implode)

let () =
  match parse_float "6.62607015e-34" with
  | Success (r, _) -> print_float (Float.of_string r)
  | Failure -> print_endline "failed"
