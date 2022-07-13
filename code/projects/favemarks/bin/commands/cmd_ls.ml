open Core
open Common

let list_all () =
  let limit = 6 in
  let offset = 0 in
  match Db.load ~limit ~offset with
  | Ok data ->
    Queue.iter data ~f:(fun { Common.id; url; description; category; tags; date } ->
        printf
          "%d %s %s %s %s %s\n"
          id
          url
          description
          category
          tags
          (Common.string_of_time date))
  | Error e -> print_error_msg e
;;

let list_bookmark params =
  match params with
  | None -> list_all ()
  | Some (sf, st, srf, sro) -> printf "%s %s %s %s\n" sf st srf sro
;;

let get_search_field v =
  let msg = "Enter search field (url or desc or tags): "
  and retry_msg = {|Search field should be either one of 'url' or 'desc' or 'tags': |}
  and validate input =
    [ "url"; "desc"; "tags" ]
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
         ~doc:"string One of the fields (url, desc, tag) to search against"
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
         ~doc:"string One of the fields (url, desc, tag, date) to sort against"
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
