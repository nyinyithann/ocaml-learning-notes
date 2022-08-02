type t =
  { mutable mode : Model.mode option
  ; mutable total_count : int
  ; mutable total_search_count : int option
  ; mutable total_pages : int
  ; mutable current_page : int
  ; mutable page_size : int
  ; mutable bookmarks : Model.bookmark list
  }

let create () =
  { mode = None
  ; total_count = 0
  ; total_search_count = None
  ; total_pages = 0
  ; current_page = 0
  ; page_size = Config_store.get_page_size ()
  ; bookmarks = []
  }
;;

(* let set_state state ?mode ~total_count ?search_count ~page_size ~bookmarks () = *)
(*   state.mode <- mode; *)
(*   state.total_count <- total_count; *)
(*   state.total_search_count <- search_count; *)
(*   state.total_pages *)
(*     <- (Float.to_int *)
(*        @@ Float.(round_up (float_of_int total_count / float_of_int page_size))); *)
(*   state.page_size <- page_size; *)
(*   state.bookmarks <- bookmarks *)
(* ;; *)

let get_mode state = state.mode
let set_mode state mode = state.mode <- mode
let get_total_count state = state.total_count
let set_total_count state count = state.total_count <- count
let get_search_count state = state.total_search_count
let set_search_count state count = state.total_search_count <- count
let get_total_pages state = state.total_pages
let set_total_pages state pages = state.total_pages <- pages
let get_page_size state = state.page_size
let set_page_size state page_size = state.page_size <- page_size
let get_current_page state = state.current_page
let set_current_page state page = state.current_page <- page
let get_bookmarks state = state.bookmarks
let set_bookmarks state bookmarks = state.bookmarks <- bookmarks
