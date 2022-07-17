open Core
open Common
module T = ANSITerminal

let limit = 12

let draw_line () =
  T.print_string
    [ T.Foreground T.Blue ]
    (Printf.sprintf
       "%-137s\n%!"
       (List.init 137 ~f:(fun _ -> "_") |> String.concat ~sep:""))
;;

let show_headers () =
  T.erase T.Screen;
  T.set_cursor 1 1;
  T.print_string
    [ T.Bold; T.Foreground T.Green ]
    "Favemarks: Your favourite bookmarks at your fingers.\n";
  draw_line ();
  T.print_string
    [ T.Bold; T.Foreground T.White; T.Background T.Blue ]
    (Printf.sprintf
       "  %-4s| %-26s| %-40s| %-20s| %-15s| %-20s\n%!"
       "Mnemonic"
       "Url"
       "Tags"
       "Description"
       "Category"
       "Date");
  draw_line ()
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

let list_all () =
  let page = ref 0 in
  let rec list_page offset total_count =
    match Db.load ~limit ~offset with
    | Ok data ->
      incr page;
      show_headers ();
      Queue.iter
        data
        ~f:(fun { Common.mnemonic; url; description; category; tags; date; _ } ->
          printf
            "| %-10s| %-20s| %-40s| %-20s| %-15s| %-19s|\n"
            mnemonic
            (ellipsis ~len:20 url)
            (ellipsis ~len:40 tags)
            (ellipsis ~len:20 description)
            (ellipsis ~len:15 category)
            (Common.string_of_time date);
          draw_line ());

      let total_pages =
        Float.(round_up (float_of_int total_count / float_of_int limit)) |> Float.to_int
      in
      show_page_info !page total_pages total_count;
      let prompt_msg = ref "" in
      if total_count = 0
      then prompt_msg := "No bookmarks to show."
      else (
        if !page < total_pages then prompt_msg := "\n j to go the next page.";
        if !page > 1 && !page < total_pages
        then prompt_msg := !prompt_msg ^ "\n k to go to the previous page.");
      prompt_msg := !prompt_msg ^ "\n mnemonic to open the corresponding link in browser.";
      prompt_msg := !prompt_msg ^ "\n Press j or k or mnemonic key: ";
      ask_input !prompt_msg;
      let c = Common.get_one_char () in
      if Char.equal c 'j'
      then
        if !page <> total_pages
        then list_page (!page * limit) total_count
        else (
          decr page;
          list_page (!page * limit) total_count)
      else if Char.equal c 'k'
      then
        if !page > 1
        then (
          page := !page - 2;
          list_page (!page * limit) total_count)
        else if !page > 0
        then (
          decr page;
          list_page (!page * limit) total_count)
        else (
          page := 0;
          list_page (!page * limit) total_count)
      else (
        match
          Queue.find data ~f:(fun { mnemonic; _ } ->
              String.equal mnemonic (String.uppercase (Char.to_string c)))
        with
        | Some r ->
          printf "\n%!";
          Common.open_link r.url
        | _ ->
          decr page;
          list_page (!page * limit) total_count)
    | Error e -> print_error_msg e
  in
  match Db.get_total_count () with
  | Ok c -> list_page (!page * limit) c
  | Error e -> print_error_msg e
;;

let list_bookmark params =
  match params with
  | None -> list_all ()
  | Some (sf, st, srf, sro) -> printf "%s %s %s %s\n" sf st srf sro
;;

let get_search_field v =
  let msg = "Enter search field (url or desc or cat or tags): "
  and retry_msg =
    {|Search field should be either one of 'url' or 'desc' or 'cat' or 'tags': |}
  and validate input =
    [ "url"; "desc"; "cat"; "tags" ]
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
  let msg = "Enter search term: "
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
  let msg = "Enter sort field (url or desc or tags or date): "
  and retry_msg =
    {|Sort field should be either one of 'url' or 'desc' or 'tags' or 'date': |}
  and validate input =
    [ "url"; "desc"; "tags"; "date" ]
    |> List.exists ~f:(fun x -> String.equal x (String.strip input))
  in
  match v with
  | None -> ask_again_if_invalid ~validate ~msg ~retry_msg ()
  | Some x ->
    if validate x
    then x
    else ask_again_if_invalid ~validate ~retry_first:() ~msg ~retry_msg ()
;;

let get_sort_order v =
  let msg = "Enter sort order (asc or desc): "
  and retry_msg = {|Sort order should be either one of 'asc' or 'desc": |}
  and validate input =
    [ "asc"; "desc" ] |> List.exists ~f:(fun x -> String.equal x (String.strip input))
  in
  match v with
  | None -> ask_again_if_invalid ~validate ~msg ~retry_msg ()
  | Some x ->
    if validate x
    then x
    else ask_again_if_invalid ~validate ~retry_first:() ~msg ~retry_msg ()
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
         ~doc:"string One of the fields (url, desc, cat, tag) to search against"
     and search_term =
       flag
         ~full_flag_required:()
         "-search-term"
         (optional string)
         ~doc:"string Search term"
     and sort_field =
       flag
         ~full_flag_required:()
         "-sort-field"
         (optional string)
         ~doc:"string One of the fields (url, desc, cat, tag, date) to sort against"
     and sort_order =
       flag
         ~full_flag_required:()
         "-sort-order"
         (optional string)
         ~doc:"string Sort order (asc or desc)"
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
