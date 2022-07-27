open Core

let config_path = "/.favemarks.config"
let db_path_key = "Db_Path_Key"
let page_size_key = "Page_Size_Key"
let default_page_size = 12
let max_page_size = 20
let cache = Hashtbl.create (module String)
let get_config_path_full () = Sys_unix.home_directory () ^ config_path

let get_config () =
  let config_path_full = get_config_path_full () in
  match Sys_unix.file_exists config_path_full with
  | `Yes ->
    (try
       In_channel.read_lines config_path_full
       |> List.fold ~init:[] ~f:(fun acc x ->
            if String.length x > 0
            then (
              match String.split x ~on:'=' |> Common.tuple_of_first_two with
              | Some (fst, snd) -> (String.strip fst, String.strip snd) :: acc
              | None -> acc)
            else acc)
       |> Ok
     with
     | e -> Error (Exn.to_string e))
  | `No | `Unknown -> Error "Config file is not found."
;;

let get_db_path () =
  let set_cache_and_return v =
    Hashtbl.set cache ~key:db_path_key ~data:v;
    Ok v
  in
  match Hashtbl.find cache db_path_key with
  | Some v -> set_cache_and_return v
  | None ->
    (match get_config () with
     | Ok l ->
       (match
          List.find l ~f:(fun (fst, _) ->
            String.(equal (uppercase (strip fst)) (uppercase db_path_key)))
        with
        | Some (_, snd) -> set_cache_and_return (String.strip snd)
        | _ ->
          Error
            "Db path is not found in config file. You need to create a db by running \
             \'fm db -new \'.Or you can point to the existing db path by running \'fm \
             config -switch\'.")
     | Error _ as e -> e)
;;

let get_page_size () =
  let set_cache_and_return v =
    let pz =
      match int_of_string_opt (String.strip v) with
      | Some x ->
        if x > max_page_size
        then max_page_size
        else if x <= 0
        then default_page_size
        else x
      | None -> default_page_size
    in
    Hashtbl.set cache ~key:page_size_key ~data:(string_of_int pz);
    pz
  in
  match Hashtbl.find cache page_size_key with
  | Some v -> set_cache_and_return v
  | None ->
    (match get_config () with
     | Ok l ->
       (match
          List.find l ~f:(fun (fst, _) ->
            String.(equal (uppercase (strip fst)) (uppercase page_size_key)))
        with
        | Some (_, snd) -> set_cache_and_return snd
        | _ -> set_cache_and_return (string_of_int default_page_size))
     | _ -> set_cache_and_return (string_of_int default_page_size))
;;
