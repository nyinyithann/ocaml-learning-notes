open Core
open Sqlite3
open Core.Result

let get_db_path_check_msg () =
  sprintf "Please check db path value at \'%s\' file"
  @@ Lazy.force FMConfig.get_config_path_full
;;

let get_total_count () =
  match FMConfig.get_db_path () with
  | Ok db_uri ->
    (try
       let& db = db_open ~mode:`NO_CREATE ~uri:true db_uri in
       let sql = sprintf "SELECT COUNT(*) FROM bookmarks" in
       let stmt = prepare db sql in
       ignore @@ step stmt;
       Ok (column_int stmt 0)
     with
     | SqliteError s -> Error s
     | e -> Error (sprintf "%s. %s." (Exn.to_string e) (get_db_path_check_msg ())))
  | Error _ as e -> e
;;

let get_like_clauses search_field search_term =
  search_term
  |> String.split ~on:','
  |> List.fold ~init:"" ~f:(fun acc x ->
       let sx = String.strip x in
       sprintf "%s %s" (if Common.is_whitespace acc then acc else acc ^ " OR ")
       @@ search_field
       |> String.split ~on:','
       |> List.fold ~init:"" ~f:(fun acc y ->
            sprintf
              "%s %s LIKE \'%%%s%%\' "
              (if Common.is_whitespace acc then acc else acc ^ " OR ")
              (String.strip y)
              sx))
;;

let get_search_total_count ~search_field ~search_term =
  match FMConfig.get_db_path () with
  | Ok db_uri ->
    (try
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
     | e -> Error (sprintf "%s. %s." (Exn.to_string e) (get_db_path_check_msg ())))
  | Error _ as e -> e
;;

let generate_mnemonics () =
  let page_size = FMConfig.get_page_size () in
  Array.init (page_size + 5) ~f:(fun x -> String.of_char @@ Char.of_int_exn (x + 97))
  |> Array.filter ~f:(fun x ->
       String.(x <> "u" && x <> "d" && x <> "q" && x <> "j" && x <> "k"))
;;

let save ~url ~tags =
  match FMConfig.get_db_path () with
  | Ok db_uri ->
    (try
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
     | e -> Error (sprintf "%s. %s." (Exn.to_string e) (get_db_path_check_msg ())))
  | Error _ as e -> e
;;

let update ~id ~url ~tags =
  match FMConfig.get_db_path () with
  | Ok db_uri ->
    (try
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
     | e -> Error (sprintf "%s. %s." (Exn.to_string e) (get_db_path_check_msg ())))
  | Error _ as e -> e
;;

let delete ~id =
  match FMConfig.get_db_path () with
  | Ok db_uri ->
    (try
       let& db = db_open ~mode:`NO_CREATE ~uri:true db_uri in
       let sql = sprintf "DELETE FROM bookmarks WHERE id = %d" id in
       let result =
         match exec db sql with
         | Rc.OK -> Ok (sprintf "Successfully deleted a record with id %d\n" id)
         | e -> Error (sprintf "%s.%s" (Rc.to_string e) (errmsg db))
       in
       result
     with
     | SqliteError s -> Error s
     | e -> Error (sprintf "%s. %s." (Exn.to_string e) (get_db_path_check_msg ())))
  | Error _ as e -> e
;;

let load ~limit ~offset ?search_field ?search_term ?sort_field ?sort_order () =
  match FMConfig.get_db_path () with
  | Ok db_uri ->
    (try
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
         | _, _, Some stf, Some sto ->
           sprintf
             "SELECT * FROM bookmarks ORDER BY %s %s LIMIT %d OFFSET %d"
             stf
             sto
             limit
             offset
         | _, _, Some stf, _ ->
           sprintf
             "SELECT * FROM bookmarks ORDER BY %s ASC LIMIT %d OFFSET %d"
             stf
             limit
             offset
         | _, _, _, Some sto ->
           sprintf
             "SELECT * FROM bookmarks ORDER BY id %s LIMIT %d OFFSET %d"
             sto
             limit
             offset
         | _ ->
           sprintf
             "SELECT * FROM bookmarks ORDER BY id DESC LIMIT %d OFFSET %d"
             limit
             offset
       in
       let stmt = prepare db sql in
       let idx = ref 0 in
       let mnemonics = generate_mnemonics () in
       while Poly.(step stmt = Rc.ROW) do
         let id = column_int stmt 0
         and url = column_text stmt 1
         and tags = column_text stmt 2
         and date = column_text stmt 3 in
         Queue.enqueue
           data_queue
           { Model.id
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
     | e -> Error (sprintf "%s. %s." (Exn.to_string e) (get_db_path_check_msg ())))
  | Error _ as e -> e
;;
