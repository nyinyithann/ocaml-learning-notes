open UI_prompt
open UI_display
open Common

let get_url v =
  let open Core in
  let msg = "Enter a url: "
  and retry_msg = "A valid url must be provided." in
  (match v with
   | None -> ask_again_if_invalid ~validate:validate_url ~msg ~retry_msg ()
   | Some x ->
     if validate_url x
     then x
     else ask_again_if_invalid ~validate:validate_url ~retry_first:() ~msg ~retry_msg ())
  |> String.strip
;;

let get_tags v =
  let open Core in
  let msg = "Enter comma-delimited tags: "
  and retry_msg =
    "One or more comma-delimited tags must be provided. Tags should not have space."
  in
  (match v with
   | None -> ask_again_if_invalid ~validate:validate_tags ~msg ~retry_msg ()
   | Some x ->
     if validate_tags x
     then String.strip x
     else ask_again_if_invalid ~validate:validate_tags ~retry_first:() ~msg ~retry_msg ())
  |> strip_space_and_concat ~sep:","
;;

let add_with_return ~url ~tags =
  new_line ();
  let url = get_url url
  and tags = get_tags tags in
  Data_store.add ~url ~tags
;;

let add ~url ~tags =
  new_line ();
  let url = get_url url
  and tags = get_tags tags in
  with_console_report ~f:(fun () -> Data_store.add ~url ~tags)
;;
