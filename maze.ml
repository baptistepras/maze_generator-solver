let () = Printf.printf"Begin...\n"

type layout = int list list

type labryinthe = {
  taille : int*int;
  walls : layout;
  depart : int*int;
  arrivee : int*int;
}



let path = "test/maze_4x8.laby"


let load_file file_path =
  let in_c = open_in file_path in
 
  let rec aux file =
    try
      let line = input_line file in
      line::aux file
    with
      End_of_file -> []  in
  aux in_c


let transforme c = 
  match c with
   | '+' | '-' | '|' -> "0"
   | _ -> "1"
  



let print_line line =
  String.iter (fun c -> let s = transforme c in Printf.printf"%s" s) line;
  Printf.printf"\n"
  
let print_list liste = List.iter (fun x -> Printf.printf"%s\n" x) liste

let print_walls liste = List.iter print_line liste
let file = load_file path

let () =print_walls file




let fin = Printf.printf"End...\n"