open Core

type state =
  { mutable mode : Model.mode option
  ; mutable total_count : int
  ; mutable total_search_count : int option
  ; mutable total_pages : int
  ; mutable current_page : int
  ; mutable page_size : int
  ; mutable bookmarks : Model.bookmark list
  }

let state : state =
  { mode = None
  ; total_count = 0
  ; total_search_count = None
  ; total_pages = 0
  ; current_page = 0
  ; page_size = 0
  ; bookmarks = []
  }
;;

let set_mode mode = state.mode <- mode
let get_state () = state

let set_state ?mode ~total_count ?search_count ~page_size ~bookmarks () =
  state.mode <- mode;
  state.total_count <- total_count;
  state.total_search_count <- search_count;
  state.total_pages
    <- (Float.to_int
       @@ Float.(round_up (float_of_int total_count / float_of_int page_size)));
  state.page_size <- page_size;
  state.bookmarks <- bookmarks
;;

let with_db_path (type a) ~(f : string -> (a, string) result) =
  match Config_store.get_db_path () with
  | Ok p -> f p
  | Error _ as e -> e
;;

let add ~url ~tags = with_db_path ~f:(fun db_path -> Db.add ~db_path ~url ~tags)
let get_total_count () = with_db_path ~f:(fun db_path -> Db.get_total_count ~db_path)

let get_search_total_count ~search_field ~search_term =
  with_db_path ~f:(fun db_path ->
    Db.get_search_total_count ~db_path ~search_field ~search_term)
;;

let delete ~id = with_db_path ~f:(fun db_path -> Db.delete ~db_path ~id)

let update ~id ~url ~tags =
  with_db_path ~f:(fun db_path -> Db.update ~db_path ~id ~url ~tags)
;;

let search ~mode =
  match mode with
  | Model.Search { search_field; search_term; sort_field; sort_order } ->
    let ( let* ) = Result.( >>= ) in
    let* total_count = get_total_count () in
    let* search_count = get_search_total_count ~search_field ~search_term in
    let page_size = Config_store.get_page_size () in
    let offset = state.current_page * page_size in
    let* data =
      with_db_path ~f:(fun db_path -> Db.load ~db_path ~mode ~limit:page_size ~offset)
    in
    set_state
      ?mode:(Some mode)
      ~total_count
      ?search_count:(Some search_count)
      ~page_size
      ~bookmarks:(Queue.to_list data)
      ();
    Ok state
  | _ -> Error "search_field and search_term must be provided."
;;
