type t

val create : unit -> t

(* val set_state *)
(*   :  t *)
(*   -> ?mode:Model.mode *)
(*   -> total_count:int *)
(*   -> ?search_count:int *)
(*   -> page_size:int *)
(*   -> bookmarks:Model.bookmark list *)
(*   -> unit *)
(*   -> unit *)

val get_mode : t -> Model.mode option
val set_mode : t -> Model.mode option -> unit
val get_total_count : t -> int
val set_total_count : t -> int -> unit
val get_search_count : t -> int option
val set_search_count : t -> int option -> unit
val get_total_pages : t -> int
val set_total_pages : t -> int -> unit
val get_page_size : t -> int
val set_page_size : t -> int -> unit
val get_current_page : t -> int
val set_current_page : t -> int -> unit
val get_bookmarks : t -> Model.bookmark list
val set_bookmarks : t -> Model.bookmark list -> unit
