open Core
module T = ANSITerminal

type bookmark =
  { id : int
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
