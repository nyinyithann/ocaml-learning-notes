include Stdlib.Option

module Syntax = struct
  let ( let* ) = Stdlib.Option.bind
  let ( let+ ) x f = Stdlib.Option.map f x

  let ( and+ ) opt1 opt2 =
    match opt1 with
    | Some x ->
      (match opt2 with
      | Some y -> some (x, y)
      | None -> none)
    | None -> none
  ;;
end



