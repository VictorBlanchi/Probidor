type pos = int * int
type wall = { horizontal : bool; length : int; pos : pos }

exception Wall_out_of_bounds
exception Wall_overlap
exception Wall_missing

module Vertex : Graph.Sig.COMPARABLE with type t = pos = struct
  type t = pos

  let compare = compare
  let hash = Hashtbl.hash
  let equal = ( = )
end

(* A module for undirected graphs with vertices of type pos. *)
module Grph = Graph.Imperative.Graph.Concrete (Vertex)
module Dfs = Graph.Traverse.Dfs (Grph)

type t = { graph : Grph.t; rows : int; columns : int }
(** The type of boards. 
    The vertices of the graph represent the cells of the board, 
    and the edges represent the valid moves between adjacent cells. 
    In particular adding a wall involves removing edges from the graph. *)

let rows board = board.rows
let columns board = board.columns
let add_pos (p1 : pos) (p2 : pos) : pos = (fst p1 + fst p2, snd p1 + snd p2)

(** Create a graph in the shape of a matrix of size rows * columns 
    where each cell is linked to its 4 direct neighbours. *)
let lattice_graph ~rows ~columns : Grph.t =
  let graph = Grph.create () in
  for i = 0 to rows - 1 do
    for j = 0 to columns - 1 do
      (* Add the vertex. *)
      Grph.add_vertex graph (i, j);
      (* Add the edges incident to this vertex. Remember the graph is undirected. *)
      if i > 0 then Grph.add_edge graph (i, j) (i - 1, j);
      if j > 0 then Grph.add_edge graph (i, j) (i, j - 1)
    done
  done;
  graph

let make ~rows ~columns : t =
  { graph = lattice_graph ~rows ~columns; rows; columns }

let wall_edges wall : (pos * pos) list =
  let i, j = wall.pos in
  if wall.horizontal then
    List.init wall.length (fun idx -> ((i, j + idx), (i + 1, j + idx)))
  else List.init wall.length (fun idx -> ((i + idx, j), (i + idx, j + 1)))

let pos_in_board board (i, j) =
  0 <= i && i < board.rows && 0 <= j && j < board.columns

let wall_in_board board wall : bool =
  let i, j = wall.pos in
  if wall.horizontal then
    pos_in_board board (i, j) && pos_in_board board (i + 1, j + wall.length - 1)
  else
    pos_in_board board (i, j) && pos_in_board board (i + wall.length - 1, j + 1)

let add_wall board wall =
  if not (wall_in_board board wall) then raise Wall_out_of_bounds
  else
    let edges = wall_edges wall in
    (* Check the new wall won't overlap an existing wall. *)
    List.iter
      (fun (p1, p2) ->
        if not (Grph.mem_edge board.graph p1 p2) then raise Wall_overlap)
      edges;
    (* Place the wall. *)
    List.iter (fun (p1, p2) -> Grph.remove_edge board.graph p1 p2) edges

let remove_wall board wall =
  if not (wall_in_board board wall) then raise Wall_out_of_bounds
  else
    let edges = wall_edges wall in
    (* Check we are removing a wall that exists indeed. *)
    List.iter
      (fun (p1, p2) ->
        if Grph.mem_edge board.graph p1 p2 then raise Wall_missing)
      edges;
    (* Remove the wall. *)
    List.iter (fun (p1, p2) -> Grph.add_edge board.graph p1 p2) edges

let reachable board start pred =
  (* To speed up the implementation, we use exceptions
     to short-circuit the DFS whenever we find a path. *)
  let exception Found in
  try
    Dfs.iter_component
      ?pre:(Some (fun pos -> if pred pos then raise Found))
      board.graph start;
    false
  with Found -> true

let exist_edge board p1 p2 = Grph.mem_edge board.graph p1 p2

let generate_walls board length_wall =
  let wall_hor =
    List.concat
      (List.init board.rows (fun i ->
           List.init board.columns (fun j ->
               { horizontal = true; length = length_wall; pos = (i, j) })))
  in
  let wall_vert =
    List.concat
      (List.init board.rows (fun i ->
           List.init board.columns (fun j ->
               { horizontal = false; length = length_wall; pos = (i, j) })))
  in
  List.append wall_hor wall_vert
