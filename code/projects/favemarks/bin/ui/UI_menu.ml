open Core
open UI_display
open UI_prompt
module T = ANSITerminal

let show_menu
  ?search_field
  ?search_term
  ?sort_field
  ?sort_order
  ~total_pages
  ~current_page
  ~total_count
  ~search
  data
  =
  let go_home =
    search ?search_field ?search_term ?sort_field ?sort_order ~current_page ~total_count
  in
  let menu_text = Queue.create () in
  let add_prompt_msg l =
    let buffer = Buffer.create 80 in
    l
    |> List.iter ~f:(fun (title, key) ->
         Buffer.add_string buffer
         @@ T.sprintf
              [ T.Foreground T.Cyan ]
              "⊜ %-8s:%s%4s"
              title
              (T.sprintf [ T.Foreground T.Blue ] "%2s" key)
              "");
    Queue.enqueue menu_text @@ Buffer.contents buffer
  in
  add_prompt_msg [ "Add", "a"; "Search", "s"; "Next", "j"; "Quit", "q" ];
  add_prompt_msg [ "Update", "u"; "List", "l"; "Previous", "k" ];
  add_prompt_msg [ "Delete", "d"; "Open", "o"; "Info", "i" ];
  (* if current_page < total_pages - 1 then add_prompt_msg "NEXT" "j"; *)
  (* if current_page > 0 && current_page < total_pages then add_prompt_msg "PREVIOUS" "k"; *)
  print_lines @@ Queue.to_list menu_text;
  new_line ();
  ask_input "Enter your choice: ";
  let c = Char.lowercase (get_one_char ()) in
  if Char.equal c 'j' && current_page < total_pages - 1
  then
    search
      ?search_field
      ?search_term
      ?sort_field
      ?sort_order
      ~current_page:(current_page + 1)
      ~total_count
      ()
  else if Char.equal c 'k' && current_page > 0
  then
    search
      ?search_field
      ?search_term
      ?sort_field
      ?sort_order
      ~current_page:(current_page - 1)
      ~total_count
      ()
  else if Char.equal c 'u'
  then
    Update_bookmark.update
      ?search_field
      ?search_term
      ?sort_field
      ?sort_order
      ~current_page
      ~total_count
      ~search
      data
  else if Char.equal c 'd'
  then Delete_bookmark.delete ~go_home ~data
  else if Char.equal c 'o'
  then Open_bookmark.open_links data
  else if Char.equal c 'q'
  then new_line ()
  else
    search
      ?search_field
      ?search_term
      ?sort_field
      ?sort_order
      ~current_page
      ~total_count
      ()
;;
