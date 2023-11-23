let () = Printf.printf"Begin...\n"


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

let print_list liste = List.iter (fun x -> Printf.printf"%s\n" x) liste

let file = load_file path

let () =print_list file


let fin = Printf.printf"End...\n"