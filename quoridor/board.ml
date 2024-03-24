
type pos = int * int

type wall =
  { horizontal: bool
  ; length: int 
  ; pos: pos }

exception Wall_out_of_bounds
exception Wall_overlap
exception Wall_missing

module Vertex : Graph.Sig.COMPARABLE with type t = pos = struct
  type t = pos
  let compare = compare 
  let hash = Hashtbl.hash
  let equal = (=)
end
(* A module for undirected graphs with vertices of type pos. *)
module Grph = Graph.Imperative.Graph.Concrete(Vertex)
module Dfs = Graph.Traverse.Dfs(Grph)

(** The type of boards. 
    The vertices of the graph represent the cells of the board, 
    and the edges represent the valid moves between adjacent cells. 
    In particular adding a wall involves removing edges from the graph. *)
type t = 
  { graph: Grph.t
  ; rows: int 
  ; columns: int }

let rows board = board.rows
let columns board = board.columns

(** Create a graph in the shape of a matrix of size rows * columns 
    where each cell is linked to its 4 direct neighbours. *)
let lattice_graph ~rows ~columns : Grph.t = 
  let graph = Grph.create () in
  for i = 0 to rows - 1 do
    for j = 0 to columns - 1 do
      (* Add the vertex. *)
      Grph.add_vertex graph (i, j);
      (* Add the edges incident to this vertex. Remember the graph is undirected. *)
      if i > 0 then Grph.add_edge graph (i, j) (i-1, j);
      if j > 0 then Grph.add_edge graph (i, j) (i, j-1);
    done
  done;
  graph

let make ~rows ~columns : t = 
  { graph = lattice_graph ~rows ~columns; rows; columns }
  
let wall_edges wall : (pos * pos) list = 
  let (i, j) = wall.pos in
  if wall.horizontal then 
    List.init wall.length begin fun idx -> 
      (i, j + idx), (i + 1, j + idx)
    end 
  else 
    List.init wall.length begin fun idx -> 
      (i + idx, j), (i + idx, j + 1)
    end
  

let pos_in_board board (i, j) = 
  0 <= i && i < board.rows && 0 <= j && j < board.columns

let wall_in_board board wall : bool = 
  let (i, j) = wall.pos in 
  if wall.horizontal then
    pos_in_board board (i, j) && pos_in_board board (i + 1, j + wall.length - 1)
  else 
    pos_in_board board (i, j) && pos_in_board board (i + wall.length - 1, j + 1)

let add_wall board wall =
  if not (wall_in_board board wall) then 
    raise Wall_out_of_bounds
  else 
    let edges = wall_edges wall in
    (* Check the new wall won't overlap an existing wall. *)
    List.iter begin fun (p1, p2) -> 
      if not (Grph.mem_edge board.graph p1 p2) then raise Wall_overlap
    end edges;
    (* Place the wall. *)
    List.iter begin fun (p1, p2) ->
      Grph.remove_edge board.graph p1 p2
    end edges

let remove_wall board wall =  
  if not (wall_in_board board wall) then 
    raise Wall_out_of_bounds
  else 
    let edges = wall_edges wall in
    (* Check we are removing a wall that exists indeed. *)
    List.iter begin fun (p1, p2) -> 
      if Grph.mem_edge board.graph p1 p2 then raise Wall_missing
    end edges;
    (* Remove the wall. *)
    List.iter begin fun (p1, p2) ->
      Grph.add_edge board.graph p1 p2
    end edges

let reachable board start pred =
  (* To speed up the implementation, we use exceptions 
     to short-circuit the DFS whenever we find a path. *)
  let exception Found in
  try 
    Dfs.iter_component ?pre:(Some begin fun pos -> 
      if pred pos then raise Found
    end) board.graph start;
    false
  with 
    | Found -> true