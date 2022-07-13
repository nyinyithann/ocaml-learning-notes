val save
  :  url:string
  -> description:string
  -> category:string
  -> tags:string
  -> (string, string) result

val load : limit:int -> offset:int -> (Common.bookmark Base.Queue.t, string) result
