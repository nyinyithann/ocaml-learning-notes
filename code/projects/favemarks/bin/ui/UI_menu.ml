open Core
open UI_display
open UI_prompt
open Common
module T = ANSITerminal

let show_menu ~state ~ls ~search ~fn_aux () =
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
  print_lines @@ Queue.to_list menu_text;
  new_line ();
  ask_input "Your choice: ";
  let c = Char.lowercase (get_one_char ()) in

  let current_page = State.get_current_page state in
  let total_pages = State.get_total_pages state in

  if Char.(c = 'a')
  then (
    let r = Add_bookmark.add_with_return ~url:None ~tags:None in
    State.set_status state (result_to_msg_opt r);
    fn_aux ~state)
  else if Char.(c = 'j') && current_page < total_pages - 1
  then (
    State.set_current_page state (State.get_current_page state + 1);
    State.set_status state None;
    fn_aux ~state)
  else if Char.(c = 'k') && current_page > 0
  then (
    State.set_current_page state (State.get_current_page state - 1);
    State.set_status state None;
    fn_aux ~state)
  else if Char.(c = 's')
  then search ~search_term:None ~search_field:None ~sort_field:None ~sort_order:None ()
  else if Char.(c = 'l')
  then ls ?sort_field:None ?sort_order:None ()
  else if Char.(c = 'u')
  then
    (* Update_bookmark.update *)
    (*   ?search_field *)
    (*   ?search_term *)
    (*   ?sort_field *)
    (*   ?sort_order *)
    (*   ~current_page *)
    (*   ~total_count *)
    (*   ~search *)
    (*   data *)
    ()
  else if Char.(c = 'd')
  then Delete_bookmark.delete ~go_home:fn_aux ~state
  else if Char.(c = 'o')
  then (
    State.set_status state @@ Some (Open_bookmark.open_links ~state);
    fn_aux ~state)
  else if Char.(c = 'q')
  then new_line ()
  else fn_aux ~state
;;
