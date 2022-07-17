val save
  :  url:string
  -> description:string
  -> category:string
  -> tags:string
  -> (string, string) result

val get_total_count : unit -> (int, string) result
val load : limit:int -> offset:int -> (Common.bookmark Base.Queue.t, string) result
