open Core
module T = ANSITerminal

type bookmark =
  { id : int
  ; mnemonic : string
  ; url : string
  ; tags : string
  ; date : Time_unix.t
  }

let new_line () = printf "\n%!"

let ask_input msg =
  T.print_string [ T.Foreground T.Blue ] (sprintf "» %s" msg);
  printf "%!"
;;

let ask_retry msg = T.print_string [ T.Foreground T.Magenta ] (sprintf " 💪  %s\n%!" msg)

let print_ok_msg msg =
  T.print_string [ T.Foreground T.Green ] (sprintf "\n ✅  %s\n%!" msg)
;;

let print_error_msg msg =
  T.print_string [ T.Foreground T.Red ] (sprintf "\n 🌶  %s\n\n%!" msg)
;;

let print_noti msg =
  T.print_string [ T.Foreground T.Magenta ] (sprintf " 🟠  %s" msg);
  new_line ()
;;

let print_lines l = l |> List.iter ~f:(printf "🔵 %s\n%!")

let ask_again_if_invalid ?validate ?retry_first ~msg ~retry_msg () =
  let rec aux () =
    if Option.is_some retry_first
    then (
      new_line ();
      ask_retry retry_msg;
      ask_input msg)
    else ask_input msg;
    match In_channel.input_line In_channel.stdin with
    | None | Some "" ->
      ask_retry retry_msg;
      aux ()
    | Some x ->
      (match validate with
       | Some f ->
         if f x
         then x
         else (
           ask_retry retry_msg;
           aux ())
       | None -> x)
  in
  aux ()
;;

let ask_again_or_default ?validate ~msg ~retry_msg default =
  let rec aux () =
    ask_input msg;
    match In_channel.input_line In_channel.stdin with
    | None | Some "" -> default
    | Some x ->
      (match validate with
       | Some f ->
         if f x
         then x
         else (
           ask_retry retry_msg;
           aux ())
       | None -> x)
  in
  aux ()
;;

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

let get_one_char () =
  let termio = Caml_unix.tcgetattr Caml_unix.stdin in
  let () =
    Caml_unix.tcsetattr
      Caml_unix.stdin
      Caml_unix.TCSADRAIN
      { termio with Caml_unix.c_icanon = false }
  in
  let res = Caml.input_char Caml.stdin in
  Caml_unix.tcsetattr Core_unix.stdin Caml_unix.TCSADRAIN termio;
  res
;;

let open_link link =
  if not (is_whitespace link)
  then (
    let link =
      if String.is_prefix ~prefix:"http://" link
         || String.is_prefix ~prefix:"https://" link
      then link
      else "https://" ^ link
    in
    Caml_unix.execvp "open" [| "open"; "-a"; "Google Chrome"; link |])
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
