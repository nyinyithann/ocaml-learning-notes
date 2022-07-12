open Core
open Core_compat

let cmd_group =
  Command.group
    ~summary:"🐫 A command-line app to store your favourite bookmarks 🐫"
    [ "save", Cmd_save.command ]
;;

let () = Command_unix.run ~version:"0.0.1" ~build_info:"favemarks ver 0.0.1" cmd_group
