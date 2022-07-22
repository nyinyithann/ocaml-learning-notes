open Core
open Sqlite3
open Core.Result

let db_uri =
  "/Users/jazz/ghq/github.com/nyinyithann/notes_on_ocaml/code/projects/favemarks/db/favemarks.db"
;;

let save ~url ~tags =
  try
    let& db = db_open ~mode:`NO_CREATE ~uri:true db_uri in
    let sql =
      sprintf
        "INSERT INTO bookmarks(url, tags, date) VALUES('%s', '%s', '%s')"
        url
        tags
        (Time.now () |> Time.to_string_utc)
    in
    let result =
      match exec db sql with
      | Rc.OK -> Ok (sprintf "Successfully saved with id %Ld\n" (last_insert_rowid db))
      | e -> Error (sprintf "%s.%s" (Rc.to_string e) (errmsg db))
    in
    result
  with
  | SqliteError s -> Error s
  | e -> Error (Exn.to_string e)
;;

let update ~id ~url ~tags =
  try
    let& db = db_open ~mode:`NO_CREATE ~uri:true db_uri in
    let sql =
      sprintf
        "UPDATE bookmarks SET url = '%s', tags = '%s', date = '%s' WHERE id = %d"
        url
        tags
        (Time.now () |> Time.to_string_utc)
        id
    in
    let result =
      match exec db sql with
      | Rc.OK -> Ok (sprintf "Successfully updated record with id %d\n" id)
      | e -> Error (sprintf "%s.%s" (Rc.to_string e) (errmsg db))
    in
    result
  with
  | SqliteError s -> Error s
  | e -> Error (Exn.to_string e)
;;

let get_total_count () =
  try
    let& db = db_open ~mode:`NO_CREATE ~uri:true db_uri in
    let sql = sprintf "SELECT COUNT(*) FROM bookmarks" in
    let stmt = prepare db sql in
    ignore @@ step stmt;
    Ok (column_int stmt 0)
  with
  | SqliteError s -> Error s
  | e -> Error (Exn.to_string e)
;;

let get_like_clauses search_field search_term =
  search_term
  |> String.split ~on:','
  |> List.fold ~init:"" ~f:(fun acc x ->
       Printf.sprintf
         "%s %s LIKE \'%%%s%%\'"
         (if Common.is_whitespace acc then acc else acc ^ " OR ")
         search_field
         (String.strip x))
;;

let get_search_total_count ~search_field ~search_term =
  try
    let& db = db_open ~mode:`NO_CREATE ~uri:true db_uri in
    let sql =
      sprintf
        "SELECT COUNT(*) FROM bookmarks WHERE %s"
        (get_like_clauses search_field search_term)
    in
    let stmt = prepare db sql in
    ignore @@ step stmt;
    Ok (column_int stmt 0)
  with
  | SqliteError s -> Error s
  | e -> Error (Exn.to_string e)
;;

let mnemonics = [| "a"; "s"; "d"; "f"; "b"; "c"; "e"; "w"; "r"; "i"; "x"; "y" |]

let load ~limit ~offset ?search_field ?search_term ?sort_field ?sort_order () =
  try
    let& db = db_open ~mode:`NO_CREATE ~uri:true db_uri in
    let data_queue = Queue.create ~capacity:limit () in
    let sql =
      match search_field, search_term, sort_field, sort_order with
      | Some sf, Some st, Some stf, Some sto ->
        sprintf
          "SELECT * FROM bookmarks WHERE %s ORDER BY %s %s LIMIT %d OFFSET %d"
          (get_like_clauses sf st)
          stf
          sto
          limit
          offset
      | Some sf, Some st, Some stf, _ ->
        sprintf
          "SELECT * FROM bookmarks WHERE %s ORDER BY %s asc LIMIT %d OFFSET %d"
          (get_like_clauses sf st)
          stf
          limit
          offset
      | Some sf, Some st, _, _ ->
        sprintf
          "SELECT * FROM bookmarks WHERE %s ORDER BY id desc LIMIT %d OFFSET %d"
          (get_like_clauses sf st)
          limit
          offset
      | _ ->
        sprintf "SELECT * FROM bookmarks ORDER BY id DESC LIMIT %d OFFSET %d" limit offset
    in
    let stmt = prepare db sql in
    let idx = ref 0 in
    while Poly.( = ) (step stmt) Rc.ROW do
      let id = column_int stmt 0
      and url = column_text stmt 1
      and tags = column_text stmt 2
      and date = column_text stmt 3 in
      Queue.enqueue
        data_queue
        { Common.id
        ; mnemonic = mnemonics.(!idx)
        ; url
        ; tags
        ; date = Common.time_of_string date
        };
      incr idx
    done;
    Ok data_queue
  with
  | SqliteError s -> Error s
  | e -> Error (Exn.to_string e)
;;
