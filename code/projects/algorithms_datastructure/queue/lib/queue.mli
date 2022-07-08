type 'a t

exception Empty

val create : unit -> 'a t

val is_empty : 'a t -> bool

val length : 'a t -> int

val enqueue : 'a -> 'a t -> 'a t

val dequeue : 'a t -> 'a t

val dequeue_opt : 'a t -> 'a t option

val peek : 'a t -> 'a

val peek_opt : 'a t -> 'a option

val iter : f:('a -> unit) -> 'a t -> unit

val fold : f:('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

val to_list : 'a t -> 'a list

val of_list : 'a list -> 'a t

val to_seq : 'a t -> 'a Seq.t

val of_seq : 'a Seq.t -> 'a t
