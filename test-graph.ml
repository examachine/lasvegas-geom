
type graph = Digraph.digraph

let g : graph = Digraph.make ()

let main () =
  print_string "test-graph\n";
  Digraph.add g (1,3);
  Digraph.add g (0,2);
  Digraph.add g (0,1);
  Printf.printf "E=%s \n" (Digraph.to_string g);
  Digraph.add g (2,4);
  Digraph.add g (0,4);
  Digraph.add g (2,6);
  Digraph.add g (4,0);
  Digraph.add g (6,5);
  Digraph.add g (5,4);
  Digraph.add g (5,1);
  Digraph.add g (3,5);
  Printf.printf "E=%s \n" (Digraph.to_string g);
  Printf.printf "number of vertices %d \n" (Digraph.num_vertices g);
  Printf.printf "number of edges %d \n" (Digraph.num_edges g);
  Printf.printf "is (2,6) in? %b \n" (Digraph.edge_in g (2,6));
  Digraph.remove g (2,6);
  Digraph.remove g (4,0);
  Digraph.remove g (5,0); (*no such edge*)
  Printf.printf "E=%s \n" (Digraph.to_string g);
  Printf.printf "is (2,6) in? %b \n" (Digraph.edge_in g (2,6));
  Printf.printf "number of vertices %d \n" (Digraph.num_vertices g);
  Printf.printf "number of edges %d \n" (Digraph.num_edges g);
  exit 0;;

main();
