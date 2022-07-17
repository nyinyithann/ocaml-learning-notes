type bookmark =
  { id : int
  ; mnemonic : string
  ; url : string
  ; description : string
  ; category : string
  ; tags : string
  ; date : Time_unix.t
  }

val ask_input : string -> unit
val ask_retry : string -> unit
val print_ok_msg : string -> unit
val print_error_msg : string -> unit

val ask_again_if_invalid
  :  ?validate:(string -> bool)
  -> ?retry_first:unit
  -> msg:string
  -> retry_msg:string
  -> unit
  -> string

val is_whitespace : string -> bool
val time_of_string : string -> Time_unix.t
val string_of_time : Time_unix.t -> string
val strip_space : string -> string
val ellipsis : len:int -> string -> string
val get_one_char : unit -> char
val open_link : string -> unit
