open Core
open Common
module T = ANSITerminal

let limit = 12

let draw_line () =
  T.print_string
    [ T.Foreground T.Blue ]
    (Printf.sprintf
       "%-161s\n%!"
       (List.init 161 ~f:(fun _ -> "_") |> String.concat ~sep:""))
;;

let show_headers () =
  T.erase T.Screen;
  T.set_cursor 1 1;
  T.print_string
    [ T.Bold; T.Foreground T.Green ]
    "Favemarks: Your favourite bookmarks at your fingertips.\n";
  draw_line ();
  T.print_string
    [ T.Bold; T.Foreground T.White; T.Background T.Blue ]
    (Printf.sprintf
       "| %-4s| %-7s| %-60s| %-60s| %-19s|\n%!"
       "Open"
       "Id"
       "Url"
       "Tags"
       "Date");
  draw_line ()
;;

let show_empty () =
  show_headers ();
  T.print_string [ T.Foreground T.Red ] (Printf.sprintf "No bookmarks to display.\n%!")
;;

let show_page_info current_page total_pages total_count =
  T.print_string
    [ T.Foreground T.Green ]
    (Printf.sprintf
       "[Page %d/%d] [Total bookmarks: %d]\n\n%!"
       current_page
       total_pages
       total_count)
;;

let get_id data =
  let msg = "Enter id to update: "
  and retry_msg = "id not found. Please try again."
  and validate input =
    Queue.exists data ~f:(fun x ->
        String.equal (string_of_int x.Common.id) (String.strip input))
  in
  ask_again_if_invalid ~validate ~msg ~retry_msg ()
;;

let rec list_bookmark_page ?search_field ?search_term ?sort_field ?sort_order total_count =
  let page = ref 0 in
  let offset = !page * limit in
  match Db.load ~limit ~offset ?search_field ?search_term ?sort_field ?sort_order () with
  | Ok data ->
    incr page;
    show_headers ();
    Queue.iter data ~f:(fun { Common.mnemonic; id; url; tags; date; _ } ->
        printf
          "| %-4s| %-7d| %-60s| %-60s| %-19s|\n"
          mnemonic
          id
          (ellipsis ~len:60 url)
          (ellipsis ~len:60 tags)
          (Common.string_of_time date);
        draw_line ());

    let total_pages =
      Float.(round_up (float_of_int total_count / float_of_int limit)) |> Float.to_int
    in
    show_page_info !page total_pages total_count;
    let prompt_msg = ref "" in
    if !page < total_pages then prompt_msg := "\n To go to next page, press j.";
    if !page > 1 && !page < total_pages
    then prompt_msg := !prompt_msg ^ "\n To go to the previous page, press k.";
    prompt_msg
      := !prompt_msg
         ^ "\n\
           \ To open a url in the default browser, press the letter in \'Open\' column.\n\
           \ To update a record, press u.\n\
           \ To delete a record, press d.\n\
           \ To quit, press q.";
    prompt_msg := !prompt_msg ^ "\n Enter your choice: ";
    ask_input !prompt_msg;
    let c = Char.lowercase (Common.get_one_char ()) in
    if Char.equal c 'j'
    then
      if !page <> total_pages
      then
        list_bookmark_page ?search_field ?search_term ?sort_field ?sort_order total_count
      else (
        decr page;
        list_bookmark_page ?search_field ?search_term ?sort_field ?sort_order total_count)
    else if Char.equal c 'k'
    then
      if !page > 1
      then (
        page := !page - 2;
        list_bookmark_page ?search_field ?search_term ?sort_field ?sort_order total_count)
      else if !page > 0
      then (
        decr page;
        list_bookmark_page ?search_field ?search_term ?sort_field ?sort_order total_count)
      else (
        page := 0;
        list_bookmark_page ?search_field ?search_term ?sort_field ?sort_order total_count)
    else if Char.equal c 'u'
    then (
      printf "\n%!";
      let id = get_id data in
      match Queue.find data ~f:(fun x -> String.equal (string_of_int x.Common.id) id) with
      | Some { Common.url; tags; _ } ->
        let ask_modified_url () =
          T.print_string
            [ T.Foreground T.Magenta ]
            (sprintf " 🟠  Existing url: %s \n%!" url);
          let msg = "Enter modified url or nothing to skip: "
          and retry_msg = "A valid url must be provided." in
          ask_again_or_default ~validate:validate_url ~msg ~retry_msg url
        and ask_modified_tags () =
          T.print_string
            [ T.Foreground T.Magenta ]
            (sprintf " 🟠  Existing tags: %s \n%!" tags);
          let msg = "Enter modified tags or nothing to skip: "
          and retry_msg = "One or more tags must be provided." in
          ask_again_or_default ~msg ~retry_msg tags
        in
        let modified_url = ask_modified_url () in
        let modified_tags = ask_modified_tags () in
        (match Db.update ~id:(int_of_string id) ~url:modified_url ~tags:modified_tags with
        | Result.Ok s -> print_ok_msg s
        | Result.Error e -> print_error_msg e)
      | _ -> ())
    else if Char.equal c 'q'
    then (
      printf "\n%!";
      ())
    else (
      match
        Queue.find data ~f:(fun { mnemonic; _ } ->
            String.equal (String.lowercase mnemonic) (String.lowercase (Char.to_string c)))
      with
      | Some r ->
        printf "\n%!";
        (match Caml_unix.fork () with
        | 0 -> Common.open_link r.url
        | _ ->
          decr page;
          list_bookmark_page
            ?search_field
            ?search_term
            ?sort_field
            ?sort_order
            total_count)
      | _ ->
        decr page;
        list_bookmark_page ?search_field ?search_term ?sort_field ?sort_order total_count)
  | Error e -> print_error_msg e
;;

let list_bookmarks () =
  match Db.get_total_count () with
  | Ok c -> if c = 0 then show_empty () else list_bookmark_page c
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
    else list_bookmark_page ~search_field ~search_term ?sort_field ?sort_order c
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
    ~summary:"List or search bookmarks."
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
            (List.exists
               ~f:Option.is_some
               [ search_field; search_term; sort_field; sort_order ])
       then None
       else get_params ~search_field ~search_term ~sort_field ~sort_order
     in
     fun () -> list_bookmark params)
;;
