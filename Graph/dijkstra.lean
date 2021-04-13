import Graph.graphrepresentation
import Std.Data.HashSet

namespace Graph

variable {α : Type} [BEq α] [Inhabited α]

inductive Path (α : Type _) : Bool → Type _ where
 | vertex : Vertex α -> Path α false -> Path α true
 | edge : Nat -> Path α true -> Path α false
 | empty : Path α false

def findMinimum (set : Std.HashSet Nat) (distances : Array ((Option Nat) × Nat)) : Nat := 
  let min : Nat -> Nat -> Nat := fun leftId rightId => 
    let leftDistance := distances[leftId].1
    let rightDistance := distances[rightId].1
    match rightDistance with
      | none => leftId
      | some r => match leftDistance with
        | none => rightId
        | some l => if l < r then leftId else rightId

  -- Question : can I remove this somehow and get any member of the set without transforming to array?
  let arr := set.toArray
  let initial := arr[0]
  set.fold min initial

-- Note for thesis :Fuel pattern - give enough fuel to always treminate
def dijkstraAux (g : Graph α) (current : Nat) (unvisited : Std.HashSet Nat) (distanceAndPredecessor : Array ((Option Nat) × Nat)) : Nat -> Array ((Option Nat) × Nat)
  | 0 => return distanceAndPredecessor
  | (n + 1) => do
    let mut distances : Array ((Option Nat) × Nat) := distanceAndPredecessor
    for edge in g.vertices[current].adjacencyList do
      if unvisited.contains edge.target then
        let tentativeDistance : Nat := match distances[current].1 with
          | some x => x + edge.weight.toNat
          | none => panic! "Current node has no distance assigned, this should not be possible"
        distances := match distances[edge.target].1 with
          | some x => if tentativeDistance < x then distances.set! edge.target (some tentativeDistance, current) else distances
          | none => distances.set! edge.target (some tentativeDistance, current)
    let nextCurrent : Nat := findMinimum unvisited distances
    match distances[nextCurrent].1 with
     | none => distances
     | some x => dijkstraAux g nextCurrent (unvisited.erase nextCurrent) distances n


def dijkstraUnsafe (g : Graph α) (source : Nat) : Array ((Option Nat) × Nat) :=
  let distanceAndPredecessorInitial : Array ((Option Nat) × Nat) := mkArray g.vertices.size (none, source) -- (weight, parent) pairs initialized to (infinitiy, placeholder)
  if h : source < distanceAndPredecessorInitial.size
    then
      let distanceAndPredecessor := distanceAndPredecessorInitial.set ⟨source, h⟩ (some 0, source)
      let unvisitedSet : Std.HashSet Nat := do
        let mut temp : Std.HashSet Nat := Std.HashSet.empty
        for i in [0:g.vertices.size] do temp := temp.insert i
        temp
      dijkstraAux g source (unvisitedSet.erase source) distanceAndPredecessor (unvisitedSet.size-1)
    else 
      panic! "source out of bounds"


-- def dijkstraSafe TODO


-- TODO: dijkstra with destination specified - question: should I "short circuit" it or is it fine if it depends on the shortest path tree implementation above?
-- def dijkstraAux (g : Graph α) (current : Nat) (destination : Nat) (unvisited : Std.HashSet Nat) (distanceAndPredecessor : Array ((Option Nat) × Nat)) : Nat -> Option (Path α true)
-- def dijkstraUnsafe (g : Graph α) (source : Nat) (destination : Nat) : Option (Path α true) :=


end Graph