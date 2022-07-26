open Core
open Common
open UI_display
open UI_prompt
module T = ANSITerminal

let limit = 12

let columns =
  let open Ascii_table_kernel in
  [ Column.create
      ~align:Align.Left
      ~min_width:6
      ~max_width:6
      "Open"
      (fun (x : Model.bookmark) -> x.mnemonic)
  ; Column.create
      ~align:Align.Left
      ~min_width:8
      ~max_width:8
      "Id"
      (fun (x : Model.bookmark) -> string_of_int x.id)
  ; Column.create ~align:Align.Left ~min_width:35 "Url" (fun (x : Model.bookmark) ->
      x.url)
  ; Column.create ~align:Align.Left ~min_width:35 "Tags" (fun (x : Model.bookmark) ->
      x.tags)
  ; Column.create ~align:Align.Left ~min_width:20 "Date" (fun (x : Model.bookmark) ->
      string_of_time x.date)
  ]
;;

let show_title () =
  T.erase T.Screen;
  T.set_cursor 0 0;
  T.print_string
    [ T.Foreground T.Green; T.Bold ]
    (sprintf "\n ☘️ %s" "Favemarks: Your favourite bookmarks at your fingertips.");
  new_line ()
;;

let show_page_info current_page total_pages total_count =
  T.print_string
    [ T.Foreground T.Green ]
    (Printf.sprintf
       " [Page %d/%d] [Total bookmarks: %d]\n\n%!"
       (if current_page < total_pages then current_page + 1 else current_page)
       total_pages
       total_count)
;;

let get_id data =
  let msg = "Enter id to update: "
  and retry_msg = "id not found. Please try again."
  and validate input =
    Queue.exists data ~f:(fun x ->
      String.equal (string_of_int x.Model.id) (String.strip input))
  in
  ask_again_if_invalid ~validate ~msg ~retry_msg ()
;;

let rec list_bookmark_page
  ?search_field
  ?search_term
  ?sort_field
  ?sort_order
  ~page
  ~total_count
  ()
  =
  let offset = page * limit in
  match Db.load ~limit ~offset ?search_field ?search_term ?sort_field ?sort_order () with
  | Ok data ->
    show_title ();
    Ascii_table.output ~oc:stdout ~limit_width_to:140 ~bars:`Unicode columns
    @@ Queue.to_list data;
    let total_pages =
      Float.(round_up (float_of_int total_count / float_of_int limit)) |> Float.to_int
    in
    show_page_info page total_pages total_count;
    let prompt_msg = Queue.create () in
    let add_prompt_msg = Queue.enqueue prompt_msg in
    if page < total_pages - 1 then add_prompt_msg "To go to next page, press j.";
    if page > 0 && page < total_pages
    then add_prompt_msg "To go to the previous page, press k.";
    add_prompt_msg
      "To open a url in the default browser, press the letter in \'Open\' column.";
    add_prompt_msg "To update a record, press u.";
    add_prompt_msg "To delete a record, press d.";
    add_prompt_msg "To quit, press q.";
    print_lines @@ Queue.to_list prompt_msg;
    new_line ();
    ask_input "Enter your choice: ";
    let c = Char.lowercase (get_one_char ()) in
    if Char.equal c 'j' && page < total_pages - 1
    then
      list_bookmark_page
        ?search_field
        ?search_term
        ?sort_field
        ?sort_order
        ~page:(page + 1)
        ~total_count
        ()
    else if Char.equal c 'k' && page > 0
    then
      list_bookmark_page
        ?search_field
        ?search_term
        ?sort_field
        ?sort_order
        ~page:(page - 1)
        ~total_count
        ()
    else if Char.equal c 'u'
    then (
      printf "\n%!";
      let id = get_id data in
      match Queue.find data ~f:(fun x -> String.equal (string_of_int x.Model.id) id) with
      | Some { Model.url; tags; _ } ->
        let ask_modified_url () =
          print_noti (sprintf "Existing url: %s" url);
          let msg = "Enter modified url or nothing to skip: "
          and retry_msg = "A valid url must be provided." in
          ask_again_or_default ~validate:validate_url ~msg ~retry_msg url
        and ask_modified_tags () =
          print_noti (sprintf "Existing tags: %s" tags);
          let msg = "Enter modified tags or nothing to skip: "
          and retry_msg =
            "One or more comma-delimited tags must be provided. Tags should not have \
             space."
          in
          ask_again_or_default ~validate:validate_tags ~msg ~retry_msg tags
        in
        let modified_url = ask_modified_url () in
        let modified_tags = ask_modified_tags () in
        if String.(modified_url <> url || modified_tags <> tags)
        then (
          match
            Db.update ~id:(int_of_string id) ~url:modified_url ~tags:modified_tags
          with
          | Result.Ok s -> print_ok_msg s
          | Result.Error e -> print_error_msg e);
        list_bookmark_page
          ?search_field
          ?search_term
          ?sort_field
          ?sort_order
          ~page
          ~total_count
          ()
      | _ -> ())
    else if Char.equal c 'q'
    then printf "\n%!"
    else (
      (* open bookmarks in the default browser *)
      match
        Queue.find data ~f:(fun { mnemonic; _ } ->
          String.equal (String.lowercase mnemonic) (String.lowercase (Char.to_string c)))
      with
      | Some r ->
        printf "%!";
        (match Caml_unix.fork () with
         | 0 -> Common.open_link r.url
         | _ ->
           list_bookmark_page
             ?search_field
             ?search_term
             ?sort_field
             ?sort_order
             ~page
             ~total_count
             ())
      | _ ->
        list_bookmark_page
          ?search_field
          ?search_term
          ?sort_field
          ?sort_order
          ~page
          ~total_count
          ())
  | Error e -> print_error_msg e
;;

let list_bookmarks () =
  match Db.get_total_count () with
  | Ok c -> if c = 0 then show_empty () else list_bookmark_page ~page:0 ~total_count:c ()
  | Error e -> print_error_msg e
;;

let search_bookmarks
  search_field
  search_term
  (sort_field : string option)
  (sort_order : string option)
  =
  match Db.get_search_total_count ~search_field ~search_term with
  | Ok c ->
    if c = 0
    then show_empty ()
    else
      list_bookmark_page
        ~search_field
        ~search_term
        ?sort_field
        ?sort_order
        ~page:0
        ~total_count:c
        ()
  | Error e -> print_error_msg e
;;

let list_bookmark (params : (string * string * string option * string option) option) =
  match params with
  | None -> list_bookmarks ()
  | Some (sf, st, sort_field, sort_order) -> search_bookmarks sf st sort_field sort_order
;;

let get_search_field v =
  let msg = "Enter search field (id or url or tags): "
  and retry_msg = {|Search field should be either one of  'id' or 'url' or 'tags': |}
  and validate input =
    [ "id"; "url"; "tags" ]
    |> List.exists ~f:(fun x -> String.equal x (String.strip input))
  in
  match v with
  | None -> ask_again_if_invalid ~validate ~msg ~retry_msg ()
  | Some x ->
    if validate x
    then x
    else ask_again_if_invalid ~validate ~retry_first:() ~msg ~retry_msg ()
;;

let get_search_term v =
  let msg = "Enter comma-delimited search terms: "
  and retry_msg = "Search term must be provided."
  and validate input = not (is_whitespace input) in
  match v with
  | None -> ask_again_if_invalid ~validate ~msg ~retry_msg ()
  | Some x ->
    if validate x
    then x
    else ask_again_if_invalid ~validate ~retry_first:() ~msg ~retry_msg ()
;;

let get_sort_field v =
  let msg = "Enter sort field (id or url or tags or date): "
  and retry_msg =
    {|Sort field should be either one of 'id' or 'url' or 'tags' or 'date': |}
  and validate input =
    [ "id"; "url"; "tags"; "date" ]
    |> List.exists ~f:(fun x -> String.equal x (String.strip input))
  in
  match v with
  | None -> None
  | Some x ->
    if validate x
    then Some x
    else Some (ask_again_if_invalid ~validate ~retry_first:() ~msg ~retry_msg ())
;;

let get_sort_order v =
  let msg = "Enter sort order (asc or desc): "
  and retry_msg = {|Sort order should be either one of 'asc' or 'desc": |}
  and validate input =
    [ "asc"; "desc" ] |> List.exists ~f:(fun x -> String.equal x (String.strip input))
  in
  match v with
  | None -> None
  | Some x ->
    if validate x
    then Some x
    else Some (ask_again_if_invalid ~validate ~retry_first:() ~msg ~retry_msg ())
;;

let get_params ~search_field ~search_term ~sort_field ~sort_order =
  Some
    ( get_search_field search_field
    , get_search_term search_term
    , get_sort_field sort_field
    , get_sort_order sort_order )
;;

let command =
  Command.basic
    ~summary:"Search bookmarks."
    (let%map_open.Command search_field =
       flag
         ~full_flag_required:()
         "-search-field"
         (optional string)
         ~doc:"string One of the fields (url, tags) to search against"
     and sort_order =
       flag
         ~full_flag_required:()
         "-sort-order"
         (optional string)
         ~doc:"string Sort order (asc or desc)"
     and sort_field =
       flag
         ~full_flag_required:()
         "-sort-field"
         (optional string)
         ~doc:"string One of the fields (url, tags, id, date) to sort against"
     and search_term =
       flag
         ~full_flag_required:()
         "-search-term"
         (optional string)
         ~doc:"string Search term"
     in
     let params =
       if not
          @@ List.exists
               ~f:Option.is_some
               [ search_field; search_term; sort_field; sort_order ]
       then None
       else get_params ~search_field ~search_term ~sort_field ~sort_order
     in
     fun () -> list_bookmark params)
;;
