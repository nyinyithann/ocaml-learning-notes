(* open Core *)
(* open Common *)
(* open UI_display *)
(* open UI_prompt *)
(* open UI_menu *)

(* let get_search_field v = *)
(*   let msg = "Enter search field (id or url or tags or all): " *)
(*   and retry_msg = *)
(*     {|Search field should be either one of  'id' or 'url' or 'tags' or 'all': |} *)
(*   and validate input = validate_fields [ "id"; "url"; "tags"; "all" ] input in *)
(*   (match v with *)
(*    | None -> ask_again_if_invalid ~validate ~msg ~retry_msg () *)
(*    | Some x -> *)
(*      if validate x *)
(*      then x *)
(*      else ask_again_if_invalid ~validate ~retry_first:() ~msg ~retry_msg ()) *)
(*   |> strip_and_lowercase *)
(* ;; *)

(* let get_search_term v = *)
(*   let msg = "Enter comma-delimited search terms: " *)
(*   and retry_msg = "Search term must be provided." *)
(*   and validate input = not (is_whitespace input) in *)
(*   (match v with *)
(*    | None -> ask_again_if_invalid ~validate ~msg ~retry_msg () *)
(*    | Some x -> *)
(*      if validate x *)
(*      then x *)
(*      else ask_again_if_invalid ~validate ~retry_first:() ~msg ~retry_msg ()) *)
(*   |> String.strip *)
(* ;; *)

(* let get_sort_field v = *)
(*   let msg = "Enter sort field (id or url or tags or date): " *)
(*   and retry_msg = *)
(*     {|Sort field should be either one of 'id' or 'url' or 'tags' or 'date': |} *)
(*   and validate input = validate_fields [ "id"; "url"; "tags"; "date" ] input in *)
(*   (match v with *)
(*    | None -> None *)
(*    | Some x -> *)
(*      if validate x *)
(*      then Some x *)
(*      else Some (ask_again_if_invalid ~validate ~retry_first:() ~msg ~retry_msg ())) *)
(*   |> Option.map ~f:String.strip *)
(* ;; *)

(* let get_sort_order v = *)
(*   let msg = "Enter sort order (asc or desc): " *)
(*   and retry_msg = {|Sort order should be either one of 'asc' or 'desc": |} *)
(*   and validate input = validate_fields [ "asc"; "desc" ] input in *)
(*   (match v with *)
(*    | None -> None *)
(*    | Some x -> *)
(*      if validate x *)
(*      then Some x *)
(*      else Some (ask_again_if_invalid ~validate ~retry_first:() ~msg ~retry_msg ())) *)
(*   |> Option.map ~f:String.strip *)
(* ;; *)

(* let rec search ~search_term ~search_field ~sort_field ~sort_order () = *)
(*   printf "\nHello\n%!"; *)
(*   new_line (); *)
(*   let search_term = get_search_term search_term in *)
(*   let search_field = *)
(*     let sf = get_search_field search_field in *)
(*     if String.(strip_and_lowercase sf = "all") then "id, url, tags" else sf *)
(*   in *)
(*   let sort_field = get_sort_field sort_field in *)
(*   let sort_order = get_sort_order sort_order in *)
(*   let state = State.create () in *)
(*   State.set_mode *)
(*     state *)
(*     (Some (Model.Search { search_field; search_term; sort_field; sort_order })); *)
(*   ListSearch_helper.search_aux ~state ~search *)
(* ;; *)
