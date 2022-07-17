open Printf
module T = ANSITerminal
let colors =
  [T.Black; T.Red; T.Green; T.Yellow; T.Blue; T.Magenta; T.Cyan;
   T.White; T.Default]

let color_to_string = function
  | T.Black -> "black"
  | T.Red -> "red"
  | T.Green -> "green"
  | T.Yellow -> "yellow"
  | T.Blue -> "blue"
  | T.Magenta -> "magent"
  | T.Cyan -> "cyan"
  | T.White -> "white"
  | T.Default -> "def"

let color_test () =
  (* Table *)
  let print_line fore =
    printf "%6s " (color_to_string fore);
    List.iter (fun back ->
                 T.print_string [T.Foreground fore; T.Background back; ]
                 " !Text! ";
              ) colors;
    print_string "\n" in

  T.erase T.Screen;
  T.set_cursor 1 1;
  print_string "        ";
  List.iter (fun back -> printf "%6s  " (color_to_string back)) colors;
  print_string "\n";
  List.iter print_line colors;
  (* Effects *)
  T.print_string [T.Bold] "Bold ";
  T.print_string [T.Underlined] "Underlined ";
  T.print_string [T.Blink] "Blink ";
  T.print_string [T.Inverse] "Inverse ";
  T.print_string [T.Hidden] "Hidden";
  print_string "<-- hidden\n"

let () =
  printf "Testing ANSITerminal...\n%!";
  printf "In 1sec the screen will be cleared and cursor put at (1,1).%!";
  Unix.sleep 1;
  T.erase T.Screen;
  T.set_cursor 1 1;
  Unix.sleep 1;

  let x, y = T.size() in
  printf "The size of the terminal is (%i,%i).%!" x y;
  let x, y = T.pos_cursor() in
  printf "\nThe cursor position was (%i,%i).\n%!" x y;
  T.set_cursor 3 5;
  printf "*<--- set_cursor 3 5";
  T.set_cursor 1 8;
  printf "Press ENTER to temporarily move the cursor to (3,6)%!";
  
  ignore(read_line());
  T.resize 239 56 ;
  T.save_cursor();
  T.set_cursor 3 6;
  printf "*<--- set_cursor 3 6";
  T.restore_cursor();

  for i = 5 downto 1 do
    printf "%i%!" i;
    Unix.sleep 1;
    T.move_bol();
  done ;

  color_test ()
