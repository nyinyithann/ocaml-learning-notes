open Core
open Common
open UI_display
open UI_prompt

let get_id ~msg data =
  let retry_msg = "id not found in the loaded records. Please try again."
  and validate input =
    let si = strip_and_lowercase input in
    Queue.exists data ~f:(fun x -> String.(string_of_int x.Model.id = si))
    || String.(si = "q")
  in
  ask_again_if_invalid ~validate ~msg ~retry_msg ()
;;

let get_modified_url existing_url =
  print_noti (sprintf "Existing url: %s" existing_url);
  let msg = "Enter modified url or nothing to skip: "
  and retry_msg = "A valid url must be provided." in
  ask_again_or_default ~validate:validate_url ~msg ~retry_msg existing_url
;;

let get_modified_tags existing_tags =
  print_noti (sprintf "Existing tags: %s" existing_tags);
  let msg = "Enter modified tags or nothing to skip: "
  and retry_msg =
    "One or more comma-delimited tags must be provided. Tags should not have space."
  in
  ask_again_or_default ~validate:validate_tags ~msg ~retry_msg existing_tags
;;

let update
  ?search_field
  ?search_term
  ?sort_field
  ?sort_order
  ~current_page
  ~total_count
  ~search
  data
  =
  new_line ();
  let id = get_id ~msg:"Enter id to update or q to quit: " data in
  if String.(id = "q")
  then ()
  else (
    match Queue.find data ~f:(fun x -> String.equal (string_of_int x.Model.id) id) with
    | Some { Model.url; tags; _ } ->
      let modified_url = get_modified_url url in
      let modified_tags = get_modified_tags tags in
      if String.(modified_url <> url || modified_tags <> tags)
      then (
        match Db.update ~id:(int_of_string id) ~url:modified_url ~tags:modified_tags with
        | Result.Ok s -> print_ok_msg s
        | Result.Error e -> print_error_msg e);
      search
        ?search_field
        ?search_term
        ?sort_field
        ?sort_order
        ~current_page
        ~total_count
        ()
    | _ ->
      print_error_msg
        (sprintf "Record with id %s is not found in the currently loaded data." id))
;;
