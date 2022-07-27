open Core

let is_whitespace s = s |> String.strip |> String.is_empty
let epoch_str () = Time_unix.to_string Time_unix.epoch

let time_of_string str =
  try Time_unix.of_string str with
  | _ -> Time_unix.epoch
;;

let string_of_time (time : Time_unix.t) =
  try
    let time_str =
      Time_unix.format time "%H:%M" ~zone:(Lazy.force Time_unix.Zone.local)
    in
    let date_str =
      Time_unix.format time "%d/%m/%y" ~zone:(Lazy.force Time_unix.Zone.local)
    in
    let h = int_of_string @@ String.slice time_str 0 2 in
    sprintf
      "%s %2d:%s %s"
      date_str
      (if h >= 12 then h - 12 else h)
      (String.slice time_str 3 5)
      (if h >= 12 then "PM" else "AM")
  with
  | _ -> epoch_str ()
;;

let strip_space_and_concat ~sep str =
  str |> String.split ~on:',' |> List.map ~f:String.strip |> String.concat ~sep
;;

let validate_url url =
  let re =
    Re.Perl.re
      {|((http|https)://)?(www.)?[a-zA-Z0-9@:%._\+~#?&//=]{2,256}\.[a-z]{2,6}\b([-a-zA-Z0-9@:%._\+~#?&//=]*)|}
    |> Re.compile
  in
  Re.execp re url
;;

let validate_tags tags =
  (not @@ is_whitespace tags)
  && (not
     @@ String.(
          split tags ~on:','
          |> List.exists ~f:(fun x ->
               let sx = strip x in
               sx = "" || exists sx ~f:Char.is_whitespace)))
;;

let validate_fields fields input =
  fields |> List.exists ~f:(fun x -> String.(x = lowercase @@ strip input))
;;

let tuple_of_first_two l =
  match l with
  | f :: s :: _ -> Some (f, s)
  | _ -> None
;;
