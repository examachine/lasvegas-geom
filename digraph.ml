(*
**
** ocaml module implementation Digraph
**
** Description: Directed graph ADT
** implementing adjacency list representation
**
** Author: Eray Ozkural (exa) <erayo@cs.bilkent.edu.tr>, (C) 2003
**
** Copyright: See COPYING file that comes with this distribution
**
*)

open Printf

type edge = int * int
(* an edge is an ordered pair of vertices *)

type digraph = {
  adj : (int list) Dynarray.dynarray;
}
(* adjacency list representation for directed graph *)

let make () = {
  adj = Dynarray.make [];
}

(* get neighborhood of vertex u *)

let n g u = Dynarray.get g.adj u

let adj g u = n g u

let set_adj g u a= Dynarray.set g.adj u a

let degree g u = List.length (n g u)

let add g (u,v) =
  let n = n g u in
    if not (List.exists ((=) v) n) then
      Dynarray.set g.adj u (v :: n)
    else
      ()

let remove g (u,v) =
  let n = n g u in
    Dynarray.set g.adj u (List.filter ((<>) v) n)
    
let edge_in g (u,v) =
  let n = n g u in
    List.exists ((=) v) n
  
(*let vertex_in g u = u < Dynarray.length g.adj*)

let vertex_in g u = degree g u > 0

let num_edges g =
  Array.fold_left (+) 0 (Dynarray.mapa (function x -> List.length x) g.adj)

let num_vertices g = Dynarray.length g.adj

let list_to_string el lst = "[" ^ String.concat ";" (List.map el lst)
			    ^  "]"

let edges_to_string el lst = String.concat "," (List.map el lst)

let to_string g =
  let prne i u = "(" ^ string_of_int i ^ "," ^
		 string_of_int u ^ ")" in
    "{" ^ String.concat ","
    (Array.to_list (Dynarray.mapai (fun i x -> list_to_string (prne i)
				      x) g.adj))
    ^ "}"

let dot_graph g = "TODO: dot graph here"
