import Graph.Graph
import Graph.Path
import Graph.UndirectedGraph
import Std.Data.HashSet

/-!
## Dijkstra's algorithm

Use `def dijkstra` to get a shortest path tree from the source, or `def dijkstraWithTarget` to stop the traversal once a specific target has been found.
-/

namespace Graph

variable {α : Type} [Inhabited α]

structure DijkstraVertex where
  predecessor : Nat
  distance : Option Nat := none
  edgeWeightToPredecessor : Nat := 0

instance : ToString DijkstraVertex where toString dv := "Predecessor: " ++ (toString dv.predecessor) ++ ", current distance: " ++ (toString dv.distance) ++ "\n"
instance : Inhabited DijkstraVertex := ⟨ { predecessor := default } ⟩

/-!
### ShortestPathTree
`def dijkstra` returns a shortest path tree with the specified root. You can use the following algorithms to extract information from it:
-/

structure ShortestPathTree where
  dijkstraVertices : Array DijkstraVertex

namespace ShortestPathTree

instance : ToString ShortestPathTree where toString t := toString t.dijkstraVertices

/-- Returns the distance from the root of the tree to a specific node. -/
def distanceToVertex (t : ShortestPathTree) (id : Nat) : Option Nat := t.dijkstraVertices[id].distance

/-- Returns the eccentricity of the root of the shortest path tree. If the graph is disconnected, the eccentricity of all vertices is infinite by definition, then the return value is none. -/
def eccentricity (t : ShortestPathTree) : Option Nat := t.dijkstraVertices.foldr (λ dv ecc => match (dv.distance, ecc) with
  | (none, _) => none
  | (_, none) => none
  | (some distance, some eccentricity) => if distance > eccentricity then some distance else some eccentricity
) (some 0)

/-- Returns all node IDs that follow the specified node in any shortest path from the root.
    Note that this function is not efficient (linear in the order of the graph).
    If you would like to know the shortest path to a specific vertex you should rather use `pathToVertex` (linear time in number of vertices on path) or `predecessorOfVertex` (constant time). -/
def successorsOfVertexDeprecated (t : ShortestPathTree) (id : Nat) : Array Nat := Id.run do
  let mut ret : Array Nat := Array.empty
  for i in [0:t.dijkstraVertices.size] do
    let vertex := t.dijkstraVertices[i]
    ret := match vertex.distance with
     | some distance => if vertex.predecessor == id && id != i then ret.push i else ret
     | none => ret
  ret

/-- Returns the previous node on the shortest path to the specified vertex from root. -/
def predecessorOfVertex (t : ShortestPathTree) (id : Nat) : Option Nat :=
  match t.dijkstraVertices[id].distance with
    | some distance => some t.dijkstraVertices[id].predecessor
    | none => none

private def pathToVertexAux (t : ShortestPathTree) (id : Nat) (pathSoFar : Path Nat false) : Nat -> Path Nat true
  | 0 => panic! "This should not be possible" -- This case is impossible since the longest shortest path possible can contain atmost n-1 vertices
  | n + 1 =>
    let currentVertex := t.dijkstraVertices[id]
    match currentVertex.distance with
      | none => panic! "Current vertex in shortest path tree is not reachable, this should not be possible"
      | some distance =>
        let pathWithCurrentVertexAdded : Path Nat true := Path.vertex id pathSoFar
        if currentVertex.predecessor == id then pathWithCurrentVertexAdded else
        let pathWithCurrentEdgeAdded : Path Nat false := Path.edge currentVertex.edgeWeightToPredecessor pathWithCurrentVertexAdded
        pathToVertexAux t currentVertex.predecessor pathWithCurrentEdgeAdded n

/-- Returns the shortest path from the tree root to the specified vertex. -/
def pathToVertex (t : ShortestPathTree) (id : Nat) : Option (Path Nat true) := match t.dijkstraVertices[id].distance with
  | none => none
  | some distance => some (pathToVertexAux t id Path.empty t.dijkstraVertices.size)

end ShortestPathTree


private def findMinimum (set : Std.HashSet Nat) (dijkstraVertices : Array DijkstraVertex) : Nat :=
  let min : Option Nat -> Nat -> Option Nat := λ leftIdOption rightId => match leftIdOption with
    | none => some rightId
    | some leftId =>
      let leftDistance := dijkstraVertices[leftId].distance
      let rightDistance := dijkstraVertices[rightId].distance
      match rightDistance with
        | none => some leftId
        | some r => match leftDistance with
          | none => some rightId
          | some l => if l < r then some leftId else some rightId

  match set.fold min none with
    | none => panic! "this should not be possible"
    | some temp => temp

private def dijkstraAux (g : Graph α Nat) (current : Nat) (target : Option Nat) (unvisited : Std.HashSet Nat) (dijkstraVerticesTemp : Array DijkstraVertex) : Nat -> Array DijkstraVertex
  | 0 => dijkstraVerticesTemp
  | n + 1 => Id.run do
    let mut dijkstraVertices : Array DijkstraVertex := dijkstraVerticesTemp
    for edge in g.vertices[current].adjacencyList do
      if unvisited.contains edge.target then
        let tentativeDistance : Nat := match dijkstraVertices[current].distance with
          | some x => x + edge.weight
          | none => panic! "Current node has no distance assigned, this should not be possible"
        let newDijkstraVertex : DijkstraVertex := {predecessor := current, distance := tentativeDistance, edgeWeightToPredecessor := edge.weight}
        dijkstraVertices := match dijkstraVertices[edge.target].distance with
          | some x => if tentativeDistance < x then dijkstraVertices.set! edge.target newDijkstraVertex else dijkstraVertices
          | none => dijkstraVertices.set! edge.target newDijkstraVertex
    let nextCurrent : Nat := findMinimum unvisited dijkstraVertices
    let isTargetFound : Bool := match target with
      | none => false
      | some t => t == nextCurrent
    if isTargetFound then dijkstraVertices else
      match dijkstraVertices[nextCurrent].distance with
        | none => dijkstraVertices
        | some x => dijkstraAux g nextCurrent target (unvisited.erase nextCurrent) dijkstraVertices n

private def dijkstraAuxBase (g : Graph α Nat) (source : Nat) (target : Option Nat) : Array (DijkstraVertex) :=
  let dijkstraVerticesInitial : Array (DijkstraVertex) := mkArray g.vertexCount {predecessor := source} -- predecessor is only a placeholder here, it has no significance and will be replaced or not used
  if h : source < dijkstraVerticesInitial.size then
    let dijkstraVertices := dijkstraVerticesInitial.set ⟨source, h⟩ {predecessor := source, distance := some 0}
    let isTargetFound : Bool := match target with
      | some t => t == source
      | none => false
    if isTargetFound then dijkstraVertices
    else
      let unvisitedSet : Std.HashSet Nat := Id.run do
        let mut temp : Std.HashSet Nat := Std.HashSet.empty
        for i in g.getAllVertexIDs do temp := temp.insert i
        temp
      dijkstraAux g source target (unvisitedSet.erase source) dijkstraVertices (unvisitedSet.size-1)
  else
      panic! "source out of bounds"

/-!
### Dijkstra
-/

/-- Find shortest path tree from source. Please see ShortestPathTree documentation for more info.
    Note: To ensure non-negative weights this function currently only works on graphs with natural number edge weights.
    You may use the `def mapEdges (g : Graph α β) (f : β -> γ)` function to map your edge weights to `Nat`.-/
def dijkstra (g : Graph α Nat) (source : Nat) : ShortestPathTree := ⟨ (dijkstraAuxBase g source none) ⟩

/-- This function will terminate the shortest path search once it has found the specified target. -/
def dijkstraWithTarget (g : Graph α Nat) (source : Nat) (target : Nat) : Option (Path Nat true) :=
  let shortestPathTree : ShortestPathTree := ⟨ (dijkstraAuxBase g source (some target)) ⟩
  shortestPathTree.pathToVertex target


namespace UndirectedGraph

/-- See directed graph. -/
def dijkstra (ug : UndirectedGraph α Nat) := ug.graph.dijkstra

/-- See directed graph. -/
def dijkstraWithTarget (ug : UndirectedGraph α Nat) := ug.graph.dijkstraWithTarget

end UndirectedGraph
end Graph
