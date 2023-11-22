let () = Printf.printf"Begin..\n"

let lire_fichier chemin = 
  let fichier = open_in chemin in
  fichier

let file = lire_fichier "test/maze_2x1.laby"



let () = Printf.printf"End..\n"