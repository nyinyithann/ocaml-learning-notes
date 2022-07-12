open Core
open Sqlite3
open Core.Result

let db_uri =
  "/Users/jazz/ghq/github.com/nyinyithann/notes_on_ocaml/code/projects/favemarks/db/favemarks.db"
;;

let save ~url ~description ~category ~tags =
  try
    let db = db_open ~mode:`NO_CREATE ~uri:true db_uri in
    let close_db () = db_close db |> ignore in
    let sql =
      sprintf
        "INSERT INTO bookmarks(link, description, category, tags, added) VALUES('%s', \
         '%s', '%s', '%s', '%s')"
        url
        description
        category
        tags
        (Time.now () |> Time.to_string_utc)
    in
    let result : (string, string) result =
      match exec db sql with
      | Rc.OK -> Ok (sprintf "Successfully saved with id %Ld\n" (last_insert_rowid db))
      | e -> Error (sprintf "%s.%s" (Rc.to_string e) (errmsg db))
    in
    close_db ();
    result
  with
  | SqliteError s -> Error s
  | e -> Error (Exn.to_string e)
;;
