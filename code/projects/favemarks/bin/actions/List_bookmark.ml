open Core
open UI_display
open UI_menu
open UI_prompt
open Common

let current_page = 0

let get_sort_field v =
  let msg = "Enter sort field (id or url or tags or date): "
  and retry_msg =
    {|Sort field should be either one of 'id' or 'url' or 'tags' or 'date': |}
  and validate input = validate_fields [ "id"; "url"; "tags"; "date" ] input in
  (match v with
   | None | Some "" -> None
   | Some x ->
     if validate x
     then Some x
     else Some (ask_again_if_invalid ~validate ~msg ~retry_msg ()))
  |> Option.map ~f:String.strip
;;

let get_sort_order v =
  let msg = "Enter sort order (asc or desc): "
  and retry_msg = {|Sort order should be either one of 'asc' or 'desc": |}
  and validate input = validate_fields [ "asc"; "desc" ] input in
  (match v with
   | None | Some "" -> None
   | Some x ->
     if validate x
     then Some x
     else Some (ask_again_if_invalid ~validate ~msg ~retry_msg ()))
  |> Option.map ~f:String.strip
;;

let ls ?sort_field ?sort_order () =
  let sort_field = get_sort_field sort_field
  and sort_order = get_sort_order sort_order in
  match Db.get_total_count () with
  | Ok total_count ->
    if total_count = 0
    then show_empty ()
    else (
      let page_size = FMConfig.get_page_size () in
      let offset = current_page * page_size in
      match Db.load ~limit:page_size ~offset ?sort_field ?sort_order () with
      | Ok data ->
        let total_pages =
          Float.(round_up (float_of_int total_count / float_of_int page_size))
          |> Float.to_int
        in
        display_table ~total_count ~current_page ~total_pages (Queue.to_list data);
        show_menu
          ~total_pages
          ~current_page
          ~total_count
          ~search:Search_bookmark.search_aux
          data
      | Error e -> print_error_msg e)
  | Error e -> print_error_msg e
;;
