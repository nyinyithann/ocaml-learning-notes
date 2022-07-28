open Core
open Common
open UI_display
open UI_prompt
open UI_menu

let rec search_aux
  ?search_field
  ?search_term
  ?sort_field
  ?sort_order
  ~current_page
  ~total_count
  ()
  =
  let page_size = FMConfig.get_page_size () in
  let offset = current_page * page_size in
  match
    Db.load ~limit:page_size ~offset ?search_field ?search_term ?sort_field ?sort_order ()
  with
  | Ok data ->
    let total_pages =
      Float.(round_up (float_of_int total_count / float_of_int page_size)) |> Float.to_int
    in
    display_table ~total_count ~current_page ~total_pages (Queue.to_list data);
    show_menu
      ?search_field
      ?search_term
      ?sort_field
      ?sort_order
      ~total_pages
      ~current_page
      ~total_count
      ~search:search_aux
      data
  | Error e -> print_error_msg e
;;

let get_search_field v =
  let msg = "Enter search field (id or url or tags or all): "
  and retry_msg =
    {|Search field should be either one of  'id' or 'url' or 'tags' or 'all': |}
  and validate input = validate_fields [ "id"; "url"; "tags"; "all" ] input in
  (match v with
   | None -> ask_again_if_invalid ~validate ~msg ~retry_msg ()
   | Some x ->
     if validate x
     then x
     else ask_again_if_invalid ~validate ~retry_first:() ~msg ~retry_msg ())
  |> String.strip
  |> String.lowercase
;;

let get_search_term v =
  let msg = "Enter comma-delimited search terms: "
  and retry_msg = "Search term must be provided."
  and validate input = not (is_whitespace input) in
  (match v with
   | None -> ask_again_if_invalid ~validate ~msg ~retry_msg ()
   | Some x ->
     if validate x
     then x
     else ask_again_if_invalid ~validate ~retry_first:() ~msg ~retry_msg ())
  |> String.strip
;;

let get_sort_field v =
  let msg = "Enter sort field (id or url or tags or date): "
  and retry_msg =
    {|Sort field should be either one of 'id' or 'url' or 'tags' or 'date': |}
  and validate input = validate_fields [ "id"; "url"; "tags"; "date" ] input in
  (match v with
   | None -> None
   | Some x ->
     if validate x
     then Some x
     else Some (ask_again_if_invalid ~validate ~retry_first:() ~msg ~retry_msg ()))
  |> Option.map ~f:String.strip
;;

let get_sort_order v =
  let msg = "Enter sort order (asc or desc): "
  and retry_msg = {|Sort order should be either one of 'asc' or 'desc": |}
  and validate input = validate_fields [ "asc"; "desc" ] input in
  (match v with
   | None -> None
   | Some x ->
     if validate x
     then Some x
     else Some (ask_again_if_invalid ~validate ~retry_first:() ~msg ~retry_msg ()))
  |> Option.map ~f:String.strip
;;

let search ~search_term ~search_field ~sort_field ~sort_order () =
  let search_term = get_search_term search_term
  and search_field = get_search_field search_field
  and sort_field = get_sort_field sort_field
  and sort_order = get_sort_order sort_order in
  let sf = if String.equal search_field "all" then "id, url, tags" else search_field in
  match Db.get_search_total_count ~search_field:sf ~search_term with
  | Ok c ->
    if c = 0
    then show_empty ()
    else
      search_aux
        ~search_field:sf
        ~search_term
        ?sort_field
        ?sort_order
        ~current_page:0
        ~total_count:c
        ()
  | Error e -> print_error_msg e
;;
