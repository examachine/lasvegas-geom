
(* conflict graph type
 * regions is a map obj -> a set of regions
 * objs is a map region -> a set of regions
 *)

type t = { regions: Digraph.digraph; objects: Digraph.digraph }

(* get conflicting regions of an object *)
let regions_of cg oix = Digraph.adj cg.regions oix

(* get conflicting objects of a region *)
let objects_of cg rix = Digraph.adj cg.objects rix

(* add a conflict between an object and a region *)
let add cg oix rix =
  Digraph.add cg.regions (oix, rix);
  Digraph.add cg.objects (rix, oix)

(* remove conflict *)
let remove cg oix rix =
  Digraph.remove cg.regions (oix, rix);
  Digraph.remove cg.objects (rix, oix)

(* initialize a conflict graph,
   assume one region 0 that represents whole space *)
let make s =
  let c = { regions = Digraph.make (); objects = Digraph.make ()} in
  let add i x = add c i 0
  in
    Array.iteri add s;
    c
