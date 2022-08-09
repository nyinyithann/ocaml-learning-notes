open Core
open Common
open UI_display
open UI_prompt
open Tyxml.Html

let get_format () =
  let msg = {|Enter format to export ('json', 'md', 'html'): |}
  and retry_msg = {|Format should be one of 'json', 'md', or 'html'.|}
  and validate input = validate_fields [ "json"; "md"; "html" ] input in
  strip_and_lowercase @@ ask_again_if_invalid ~validate ~msg ~retry_msg ()
;;

let get_path ~format =
  let msg = "Enter a file path to export: "
  and retry_msg =
    sprintf
      "File path should be valid. e.g. /Users/jazz/export.%s. Please provide a valid \
       file path."
      format
  and validate input =
    (Filename.is_absolute input || Filename.is_relative input)
    && Filename.check_suffix input format
  in
  ask_again_if_invalid ~validate ~msg ~retry_msg ()
;;

let to_json ~bookmarks ~path =
  try
    let rec loop bookmarks acc =
      match bookmarks with
      | [] -> acc
      | h :: t ->
        loop t
        @@ (`Assoc
              [ "id", `Int h.Model.id
              ; "url", `String h.Model.url
              ; "tags", `String h.Model.tags
              ; "date", `String (string_of_time h.Model.date)
              ]
           :: acc)
    in
    let l = loop bookmarks [] in
    let j =
      Yojson.Safe.to_string
      @@ `Assoc
           [ "title", `String "Bookmarks exported from Favemarks"
           ; "exported_date", `String (string_of_time (Time.now ()))
           ; "bookmarks_count", `Int (List.length bookmarks)
           ; "bookmarks", `List l
           ]
    in
    Out_channel.write_all path ~data:j;
    Ok (sprintf "Bookmarks are exported to %s" path)
  with
  | _ ->
    Error
      (sprintf
         "Error occured while exporting at %s.\nPlease check if the file path is valid."
         path)
;;

let to_markdown ~bookmarks ~path =
  try
    let b = Buffer.create 100 in
    Buffer.(
      add_string b "# Bookmarks exported from Favemarks<br/>\n";
      add_string b (sprintf "#### Exported Date: %s\n" @@ string_of_time @@ Time.now ());
      add_string b (sprintf "#### Bookmarks count: %d<br/>\n" @@ List.length bookmarks));
    let rec loop bookmarks acc =
      match bookmarks with
      | [] -> acc
      | h :: t ->
        Buffer.add_string
          acc
          (sprintf
             "#### Url: [%s](%s)<br/>Tags: %s<br/>\n"
             h.Model.url
             h.Model.url
             h.Model.tags);
        loop t acc
    in
    let md_str = loop bookmarks b in
    Out_channel.write_all path ~data:(Buffer.contents md_str);
    Ok (sprintf "Bookmarks are exported to %s" path)
  with
  | _ ->
    Error
      (sprintf
         "Error occured while exporting at %s.\nPlease check if the file path is valid."
         path)
;;

let generate_main_html ~bookmarks =
let header_section =
  section
    ~a:[ a_class [ "box"; "header" ] ]
    [ span ~a:[ a_class [ "id-header" ] ] [ txt "Id" ]
    ; span ~a:[ a_class [ "url-header" ] ] [ txt "URL" ]
    ; span ~a:[ a_class [ "tag-header" ] ] [ txt "Tags" ]
    ; span ~a:[ a_class [ "date-header" ] ] [ txt "Date" ]
    ] 
    in

let rec loop bookmarks acc =
    match bookmarks with
    | [] -> acc
    | h :: t -> 
            let s = 
                section ~a:[a_class ["box"]]
    



let export ~go_home ~state =
  new_line ();
  let format = get_format () in
  let path = get_path ~format in
  let bookmarks = State.get_bookmarks state in
  if String.(format = "json")
  then (
    match to_json ~bookmarks ~path with
    | Ok s -> State.set_status state (Some (with_ok_style s))
    | Error e -> State.set_status state (Some (with_error_style e)));

  if String.(format = "md")
  then (
    match to_markdown ~bookmarks ~path with
    | Ok s -> State.set_status state (Some (with_ok_style s))
    | Error e -> State.set_status state (Some (with_error_style e)));

  go_home ~state
;;
