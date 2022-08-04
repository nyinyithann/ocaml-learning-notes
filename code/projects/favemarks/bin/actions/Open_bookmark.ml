open Core
open Common
open UI_display
open UI_prompt

type open_link =
  | Empty
  | Number of int
  | Links of string list

let get_links data =
  let r = ref Empty in
  let msg =
    "Enter a number larger than 0 or comma-delimited letters from \'Key\' column: "
  and validate input =
    let input = String.strip input in
    match int_of_string_opt input with
    | Some n ->
      if n > 0
      then (
        r := Number n;
        true)
      else false
    | None ->
      if is_whitespace input
      then (
        r := Empty;
        false)
      else (
        let lnks =
          input
          |> String.split ~on:','
          |> List.fold_right
               ~f:(fun x acc ->
                 match
                   List.find data ~f:(fun y ->
                     String.(y.Model.mnemonic = strip_and_lowercase x))
                 with
                 | Some d -> d.Model.url :: acc
                 | None -> acc)
               ~init:[]
        in
        if List.length lnks > 0
        then (
          r := Links lnks;
          true)
        else (
          r := Empty;
          false))
  in
  ignore @@ ask_again_if_invalid ~validate ~msg ~retry_msg:msg ();
  !r
;;

(* exit 255 means the command could not be executed. It's not Unix's standard convention. *)
let open_url url cin cout =
  let link =
    if String.(is_prefix ~prefix:"http://" url || is_prefix ~prefix:"https://" url)
    then url
    else "https://" ^ url
  in
  (* Unix exit code min =0 max=255 *)
  (* in_channel_length throws Illagle_seek error. Hence, work around it *)
  let get_exit_code n = 130 + if n > 120 then 120 else n in
  match Config_store.get_open_with () with
  | Ok open_with ->
    (match Browser.get_browser_name open_with with
     | Ok bn ->
       (try
          Out_channel.close cout;
          Caml_unix.execvp "open" [| "open"; "-a"; bn; link |]
        with
        | Caml_unix.Unix_error (err, _, _) ->
          let e = Caml_unix.error_message err in
          Out_channel.output_string cout e;
          Out_channel.close cout;
          exit (get_exit_code (String.length e)))
     | Error e ->
       Out_channel.output_string cout e;
       Out_channel.close cout;
       exit (get_exit_code (String.length e)))
  | Error e ->
    Out_channel.output_string cout e;
    Out_channel.close cout;
    exit (get_exit_code (String.length e))
;;

let open_links ~state =
  new_line ();
  let data = State.get_bookmarks state in
  let links =
    match get_links data with
    | Number n -> List.take data n |> List.map ~f:(fun x -> x.Model.url)
    | Links ls -> ls
    | _ -> []
  in
  let fork_open_aux l =
    let open Caml_unix in
    let fd_in, fd_out = pipe () in
    let cin = in_channel_of_descr fd_in in
    let cout = out_channel_of_descr fd_out in
    match fork () with
    | 0 -> open_url l cin cout
    | _ ->
      let _, status = wait () in
      (match status with
       | WEXITED n when n > 130 ->
         let s =
           sprintf "Error at opening %s. %s." l @@ Caml.really_input_string cin (n - 130)
         in
         In_channel.close cin;
         s
       | WEXITED n ->
         In_channel.close cin;
         sprintf
           "%s is opened%s."
           l
           (sprintf " in %s"
           @@ Option.value ~default:""
           @@ Result.ok
           @@ Config_store.get_open_with ())
       | WSIGNALED signal ->
         In_channel.close cin;
         sprintf "browser is killed by signal %d." signal
       | WSTOPPED _ ->
         In_channel.close cin;
         sprintf "browser stopped.")
  in
  let fork_open ls =
    let rec loop ls acc =
      match ls with
      | [] -> acc
      | h :: t -> loop t (fork_open_aux h :: acc)
    in
    let msgs = loop ls [] in
    String.concat ~sep:"\n" msgs
  in
  fork_open links
;;
