open Core
module T = ANSITerminal

type bookmark =
  { id : int
  ; mnemonic : string
  ; url : string
  ; description : string
  ; category : string
  ; tags : string
  ; date : Time_unix.t
  }

let ask_input msg =
  T.print_string [ T.Foreground T.Blue ] (sprintf " ✏️  %s" msg);
  printf "%!"
;;

let ask_retry msg = T.print_string [ T.Foreground T.Magenta ] (sprintf " 💪  %s\n" msg)
let print_ok_msg msg = T.print_string [ T.Foreground T.Green ] (sprintf "\n ✅  %s\n" msg)

let print_error_msg msg = T.print_string [ T.Foreground T.Red ] (sprintf "\n 🌶  %s\n" msg)

let ask_again_if_invalid ?validate ?retry_first ~msg ~retry_msg () =
  let rec aux () =
    if Option.is_some retry_first
    then (
      printf "\n";
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

let is_whitespace s = s |> String.strip |> String.is_empty
let epoch_str () = Time_unix.to_string Time_unix.epoch

let time_of_string str =
  try Time_unix.of_string str with
  | _ -> Time_unix.epoch
;;

let string_of_time (time : Time_unix.t) =
  try
    Time_unix.format time "%d/%m/%y %H:%M:%S" ~zone:(Lazy.force Time_unix.Zone.local)
  with
  | _ -> epoch_str ()
;;

let strip_space str =
  Str.split (Str.regexp "[ \n\r\x0c\t]+") str |> String.concat ~sep:" "
;;

let ellipsis ~len str =
  let l = len - 3 in
  let str_len = String.length str in
  try
    if str_len <= l
    then str
    else if l < 0
    then ""
    else String.slice str 0 (len - 3) ^ "..."
  with
  | _ -> ""
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
         || String.is_prefix ~prefix:"https://" " link"
      then link
      else "https://" ^ link
    in
    Caml_unix.execvp "open" [| "open"; "-a"; "Google Chrome"; link |])
;;
