open Core
open Sqlite3
open Core.Result

let db_uri =
  "/Users/jazz/ghq/github.com/nyinyithann/notes_on_ocaml/code/projects/favemarks/db/favemarks.db"
;;

let save ~url ~description ~category ~tags =
  try
    let& db = db_open ~mode:`NO_CREATE ~uri:true db_uri in
    let sql =
      sprintf
        "INSERT INTO bookmarks(url, description, category, tags, added) VALUES('%s', \
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
    result
  with
  | SqliteError s -> Error s
  | e -> Error (Exn.to_string e)
;;

let load ~limit ~offset =
  try
    let& db = db_open ~mode:`NO_CREATE ~uri:true db_uri in
    let data_queue = Queue.create ~capacity:limit () in
    let sql =
      sprintf "SELECT * FROM bookmarks ORDER BY id LIMIT %d OFFSET %d" limit offset
    in
    let stmt = prepare db sql in
    while Poly.( = ) (step stmt) Rc.ROW do
      let id = column_int stmt 0
      and url = column_text stmt 1
      and description = column_text stmt 2
      and category = column_text stmt 3
      and tags = column_text stmt 4
      and added = column_text stmt 5 in
      Queue.enqueue
        data_queue
        { Common.id
        ; url
        ; description
        ; category
        ; tags
        ; date = Common.time_of_string added
        }
    done;
    Ok data_queue
  with
  | SqliteError s -> Error s
  | e -> Error (Exn.to_string e)
;;
