type graph = Digraph.digraph

let make = Digraph.make

let adj = Digraph.adj

let add g (u,v) =
  Digraph.add g (u,v);
  Digraph.add g (v,u)

let remove g (u,v) =
  Digraph.remove g (u,v);
  Digraph.remove g (v,u)

