 

type position = int * int
  
module V : Graph.Sig.COMPARABLE = struct
  type t = position
  let compare = compare 
  let hash = Hashtbl.hash
  let equal = (=)
end
(* A module for undirected graphs with vertices of type position. *)
module G = Graph.Imperative.Graph.Concrete(V)
  
