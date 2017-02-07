(*
**
** ocaml module interface Digraph
**
** Description: Directed graph ADT
** implementing adjacency list representation
**
** Author: Eray Ozkural (exa) <erayo@cs.bilkent.edu.tr>, (C) 2003
**
** Copyright: See COPYING file that comes with this distribution
**
*)

type edge = int * int
(* an edge is an ordered pair of vertices *)

type digraph

val make : unit -> digraph

val adj : digraph -> int -> int list
(* adjacency of a vertex *)

val set_adj : digraph -> int -> int list -> unit
(* set adjacency of a vertex *)

val degree : digraph -> int -> int
(* query degree of a vertex *)

val add : digraph -> edge -> unit
(* add edge with given weight *)

val remove : digraph -> edge -> unit
(* remove an edge *)

val edge_in : digraph -> edge -> bool
(* query edge *)

val vertex_in : digraph -> int -> bool
(* query vertex *)

val num_edges : digraph -> int
(* query number of edges *)

val num_vertices : digraph -> int
(* query number of vertices *)

val to_string : digraph -> string

val dot_graph : digraph -> string
(* graphviz representation of the graph *)

