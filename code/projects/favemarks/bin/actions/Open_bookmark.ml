open Core
open Common

let open_url_axu link =
  if not (is_whitespace link)
  then (
    let link =
      if String.is_prefix ~prefix:"http://" link
         || String.is_prefix ~prefix:"https://" link
      then link
      else "https://" ^ link
    in
    Caml_unix.execvp "open" [| "open"; "-a"; "Google Chrome"; link |])
;;

let open_url ~ch ~data =
  match
    Queue.find data ~f:(fun { Model.mnemonic; _ } ->
      String.equal (String.lowercase mnemonic) (String.lowercase (Char.to_string ch)))
  with
  | Some r ->
    printf "%!";
    (match Caml_unix.fork () with
     | 0 -> open_url_axu r.url
     | _ -> ())
  | _ -> ()
;;
