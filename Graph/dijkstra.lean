import Graph.graphrepresentation
import Std.Data.HashSet

namespace Graph

variable {α : Type} [BEq α] [Inhabited α]

inductive Path (α : Type _) : Bool → Type _ where
 | vertex : Vertex α -> Path α false -> Path α true
 | edge : Nat -> Path α true -> Path α false
 | empty : Path α false

def findMinimum (set : Std.HashSet Nat) (distances : Array ((Option Nat) × Nat)) : Nat := 
  let arr := set.toArray
  let initial := arr[0]
  let min : Nat -> Nat -> Nat := fun x y => if x > y then y else x
  arr.foldr min initial

-- Note for thesis :Fuel pattern - give enough fuel to always treminate
def dijkstraAux (g : Graph α) (current : Nat) (destination : Nat) (unvisited : Std.HashSet Nat) (distanceAndPredecessor : Array ((Option Nat) × Nat)) : Nat -> Option (Path α true)
  | 0 => return none
  | (n + 1) => do
    let mut distances : Array ((Option Nat) × Nat) := distanceAndPredecessor
    for edge in g.vertices[current].adjacencyList do
      if unvisited.contains edge.target then
        let tentativeDistance : Nat := match distanceAndPredecessor[current].1 with
          | some x => x + edge.weight.toNat
          | none => panic! "Current node has no distance assigned, this should not be possible"
        let newDistance : Nat := match distanceAndPredecessor[edge.target].1 with
          | some x => if tentativeDistance < x then tentativeDistance else x
          | none => tentativeDistance
        distances := distances.set! edge.target (some newDistance, current)
    let nextCurrent : Nat := findMinimum distances
    dijkstraAux g nextCurrent destination (unvisited.erase nextCurrent) distances n


def dijkstraUnsafe (g : Graph α) (source : Nat) (destination : Nat) : Option (Path α true) :=
  let distanceAndPredecessorInitial : Array ((Option Nat) × Nat) := mkArray g.vertices.size (none, source) -- (weight, parent) pairs initialized to (infinitiy, placeholder)
  if h : source < distanceAndPredecessorInitial.size
    then
      -- let index : Fin (distanceAndPredecessorInitial.size) := source -- Question: how to create Fin type?    (then don't need to use set!, just set)
      let distanceAndPredecessor := distanceAndPredecessorInitial.set ⟨source, h⟩ (some 0, source)
      let unvisitedSet : Std.HashSet Nat := do
        let mut temp : Std.HashSet Nat := Std.HashSet.empty
        for i in [0:g.vertices.size] do temp := temp.insert i
        temp
      _
  -- dijkstraAux g source destination source (unvisitedSet.erase source) distanceAndPredecessor
    else 
      none


-- def dijkstraSafe TODO


end Graph