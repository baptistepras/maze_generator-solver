let () = Printf.printf"Begin..\n"

let lire_fichier chemin = 
  let fichier = open_in chemin in
  fichier

let file = lire_fichier "test/maze_2x1.laby"

let pr file = 
  let s = input_line file
  Printf.printf"%s" s;




let () = Printf.printf"End..\n"