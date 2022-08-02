open Core
module T = ANSITerminal

let new_line () = printf "\n%!"

let print_ok_msg msg =
  T.print_string [ T.Foreground T.Green ] (sprintf "\n ✅  %s\n%!" msg)
;;

let print_error_msg msg =
  T.prerr_string [ T.Foreground T.Red ] (sprintf "\n 🌶  %s\n\n%!" msg)
;;

let show_empty () = print_error_msg "No bookmarks to display."

let print_noti msg =
  T.print_string [ T.Foreground T.Magenta ] (sprintf " 🟠  %s" msg);
  new_line ()
;;

let print_lines l = l |> List.iter ~f:(printf " %s\n%!")

let show_title () =
  T.erase T.Screen;
  T.set_cursor 0 0;
  T.print_string
    [ T.Foreground T.Green; T.Bold ]
    (sprintf "\n ☘️ %s" "Favemarks: Your favourite bookmarks at your fingertips.\n")
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

let display_table state =
  let columns =
    let open Ascii_table_kernel in
    [ Column.create_attr
        ~align:Align.Left
        ~min_width:6
        ~max_width:6
        "Key"
        (fun (x : Model.bookmark) -> [ `Green; `Underscore ], x.mnemonic)
    ; Column.create_attr
        ~align:Align.Left
        ~min_width:8
        ~max_width:8
        "Id"
        (fun (x : Model.bookmark) -> [ `Blue ], string_of_int x.id)
    ; Column.create_attr
        ~align:Align.Left
        ~min_width:35
        "Url"
        (fun (x : Model.bookmark) -> [ `Blue ], x.url)
    ; Column.create_attr
        ~align:Align.Left
        ~min_width:35
        "Tags"
        (fun (x : Model.bookmark) -> [ `Blue ], x.tags)
    ; Column.create_attr
        ~align:Align.Left
        ~min_width:20
        "Date"
        (fun (x : Model.bookmark) -> [ `Blue ], Common.string_of_time x.date)
    ]
  in
  show_title ();
  Ascii_table.output
    ~oc:stdout
    ~limit_width_to:140
    ~header_attr:[ `Cyan; `Bright ]
    ~bars:`Unicode
    columns
    (State.get_bookmarks state);

  let current_page = State.get_current_page state in
  let total_pages = State.get_total_pages state in
  let total_count = State.get_total_count state in
  show_page_info current_page total_pages total_count
;;

let with_console_report ~(f : unit -> (string, string) result) =
  match f () with
  | Ok s -> print_ok_msg s
  | Error e -> print_error_msg e
;;
