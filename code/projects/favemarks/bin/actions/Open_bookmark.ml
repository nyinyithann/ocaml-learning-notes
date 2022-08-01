open Core
open Common
open UI_display
open UI_prompt

type open_link =
  | Empty
  | Number of int
  | Links of string list

(* exit 255 means the command could not be executed. It's not Unix's standard convention. *)
let open_url url =
  let link =
    if String.(is_prefix ~prefix:"http://" url || is_prefix ~prefix:"https://" url)
    then url
    else "https://" ^ url
  in
  match Config_store.get_open_with () with
  | Ok open_with ->
    (match Browser.get_browser_name open_with with
     | Ok bn ->
       (try Caml_unix.execvp "open" [| "open"; "-a"; bn; link |] with
        | Caml_unix.Unix_error (err, _, _) ->
          print_error_msg @@ sprintf "%s" (Caml_unix.error_message err);
          exit 255)
     | Error e ->
       print_error_msg e;
       exit 255)
  | Error e ->
    print_error_msg e;
    exit 255
;;

let get_links data =
  let r = ref Empty in
  let msg = "Enter a number larger than 0 or comma-delimited letters from Open column: "
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

let open_links data =
  new_line ();
  let data = Queue.to_list data in
  let links =
    match get_links data with
    | Number n -> List.take data n |> List.map ~f:(fun x -> x.Model.url)
    | Links ls -> ls
    | _ -> []
  in
  let fork_open l =
    let open Caml_unix in
    match fork () with
    | 0 -> open_url l
    | _ ->
      let _, status = wait () in
      (match status with
       | WEXITED 255 -> print_error_msg @@ sprintf "%s cannot be opened" l
       | WEXITED _ -> print_ok_msg @@ sprintf "%s: opened successfully" l
       | WSIGNALED signal ->
         print_error_msg @@ sprintf "browser is killed by signal %d" signal
       | WSTOPPED _ -> print_error_msg @@ sprintf "browser stopped")
  in
  links |> List.iter ~f:fork_open
;;
