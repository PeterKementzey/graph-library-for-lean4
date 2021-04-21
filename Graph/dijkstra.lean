import Graph.graphrepresentation
import Std.Data.HashSet

namespace Graph

variable {α : Type} [BEq α] [Inhabited α]

private structure DijkstraVertex where
  predecessor : Nat
  distance : Option Nat := none
  edgeWeightToPredecessor : Nat := 0

instance : ToString DijkstraVertex where toString dv := "Predecessor: " ++ (toString dv.predecessor) ++ ", current distance: " ++ (toString dv.distance) ++ "\n"
instance : Inhabited DijkstraVertex := ⟨ { predecessor := arbitrary } ⟩


structure ShortestPathTree where
  dijkstraVertices : Array DijkstraVertex

namespace ShortestPathTree

instance : ToString ShortestPathTree where toString t := toString t.dijkstraVertices

inductive Path : Bool → Type where
  | vertex : Nat -> Path false -> Path true
  | edge : Nat -> Path true -> Path false
  | empty : ∀ {b}, Path b

namespace Path

def toString : ∀ {b}, Path b → String
  | true, vertex id p =>
    "vertex id: " ++ ToString.toString id ++ ", " ++ toString p
  | false, edge weight p =>
    "edge weight: " ++ ToString.toString weight ++ ", " ++ toString p
  | _, empty => "∎"

instance : ToString (Path b) where
  toString p := toString p

end Path

def shortestDistanceToVertex (t : ShortestPathTree) (id : Nat) : Option Nat := t.dijkstraVertices[id].distance

-- Note: make note in documentation that this is not efficient
-- FIXME: start node is the successor of itself
def successorsOfVertex (t : ShortestPathTree) (id : Nat) : Array Nat := do
  let mut ret : Array Nat := Array.empty
  for i in [0:t.dijkstraVertices.size] do
    let vertex := t.dijkstraVertices[i]
    ret := match vertex.distance with
     | some distance => if vertex.predecessor == id then ret.push i else ret
     | none => ret
  ret

def predecessorOfVertex (t : ShortestPathTree) (id : Nat) : Option Nat :=
  match t.dijkstraVertices[id].distance with
    | some distance => some t.dijkstraVertices[id].predecessor
    | none => none

private def pathToVertexAux (t : ShortestPathTree) (id : Nat) (pathSoFar : Path false) : Nat -> Path true
  | 0 => Path.empty
  | n+1 =>
    let currentVertex := t.dijkstraVertices[id]
    match currentVertex.distance with
      | none => Path.empty -- panic! "Current vertex in shortest path tree is not reachable, this should not be possible"
      | some distance =>
        let pathWithCurrentVertexAdded : Path true := Path.vertex id pathSoFar
        let pathWithCurrentEdgeAdded : Path false := Path.edge currentVertex.edgeWeightToPredecessor pathWithCurrentVertexAdded
        pathToVertexAux t currentVertex.predecessor pathWithCurrentEdgeAdded n

def pathToVertex (t : ShortestPathTree) (id : Nat) : Option (Path true) := match t.dijkstraVertices[id].distance with
  | none => none
  | some distance => some (pathToVertexAux t id Path.empty t.dijkstraVertices.size)


end ShortestPathTree


private def findMinimum (set : Std.HashSet Nat) (dijkstraVertices : Array DijkstraVertex) : Nat :=
  let min : Option Nat -> Nat -> Option Nat := fun leftIdOption rightId => match leftIdOption with
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

-- Note for thesis :Fuel pattern - give enough fuel to always treminate
private def dijkstraAux (g : Graph α) (current : Nat) (target : Option Nat) (unvisited : Std.HashSet Nat) (dijkstraVerticesTemp : Array DijkstraVertex) : Nat -> Array DijkstraVertex
  | 0 => return dijkstraVerticesTemp
  | (n + 1) => do
    let mut dijkstraVertices : Array DijkstraVertex := dijkstraVerticesTemp
    for edge in g.vertices[current].adjacencyList do
      if unvisited.contains edge.target then
        let tentativeDistance : Nat := match dijkstraVertices[current].distance with
          | some x => x + edge.weight.toNat
          | none => panic! "Current node has no distance assigned, this should not be possible"
        let newDijkstraVertex : DijkstraVertex := {predecessor := current, distance := tentativeDistance, edgeWeightToPredecessor := edge.weight.toNat}
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


private def dijkstraAuxBase (g : Graph α) (source : Nat) (target : Option Nat) : Array (DijkstraVertex) :=
  let dijkstraVerticesInitial : Array (DijkstraVertex) := mkArray g.vertices.size {predecessor := source} -- predecessor is only a placeholder here, it has no significance and will be replaced or not used
  if h : source < dijkstraVerticesInitial.size then
    let dijkstraVertices := dijkstraVerticesInitial.set ⟨source, h⟩ {predecessor := source, distance := some 0}
    let isTargetFound : Bool := match target with
      | some t => t == source
      | none => false
    if isTargetFound then dijkstraVertices
    else
      let unvisitedSet : Std.HashSet Nat := do
        let mut temp : Std.HashSet Nat := Std.HashSet.empty
        for i in [0:g.vertices.size] do temp := temp.insert i
        temp
      dijkstraAux g source target (unvisitedSet.erase source) dijkstraVertices (unvisitedSet.size-1)
  else
      panic! "source out of bounds"

def dijkstraUnsafe (g : Graph α) (source : Nat) : ShortestPathTree := ⟨ (dijkstraAuxBase g source none) ⟩

def dijkstraUnsafeWithDestination (g : Graph α) (source : Nat) (target : Nat) : Option (ShortestPathTree.Path true) := 
  let shortestPathTree : ShortestPathTree := ⟨ (dijkstraAuxBase g source (some target)) ⟩
  shortestPathTree.pathToVertex target -- TODO check what happens if the path cannot be constructed

-- def dijkstraSafe TODO check for negative weights and other requirements ?

end Graph
