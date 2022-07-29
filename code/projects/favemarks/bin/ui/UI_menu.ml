open Core
open UI_display
open UI_prompt

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
  let menu_text = Queue.create () in
  let add_prompt_msg = Queue.enqueue menu_text in
  if current_page < total_pages - 1 then add_prompt_msg "To go to next page, press j.";
  if current_page > 0 && current_page < total_pages
  then add_prompt_msg "To go to the previous page, press k.";
  add_prompt_msg "To open a url in the default browser, press o.";
  add_prompt_msg "To update a record, press u.";
  add_prompt_msg "To delete a record, press d.";
  add_prompt_msg "To quit, press q.";
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
  then Delete_bookmark.delete data
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
