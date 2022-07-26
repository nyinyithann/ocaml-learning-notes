val save : url:string -> tags:string -> (string, string) result
val get_total_count : unit -> (int, string) result

val get_search_total_count
  :  search_field:string
  -> search_term:string
  -> (int, string) result

val load
  :  limit:int
  -> offset:int
  -> ?search_field:string
  -> ?search_term:string
  -> ?sort_field:string
  -> ?sort_order:string
  -> unit
  -> (Model.bookmark Base.Queue.t, string) result

val update : id:int -> url:string -> tags:string -> (string, string) result
