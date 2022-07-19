open Core
open Core_compat

let cmd_group =
  Command.group
    ~summary:"Your favourite bookmarks at your fingertips."
    [ "add", Cmd_add.command; "ls", Cmd_ls.command ]
;;

let () = Command_unix.run ~version:"0.0.1" ~build_info:"favemarks ver 0.0.1" cmd_group
