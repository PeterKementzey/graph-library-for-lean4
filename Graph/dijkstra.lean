import Graph.graphrepresentation
import Std.Data.HashSet

namespace Graph

variable {α : Type} [BEq α] [Inhabited α]

inductive Path (α : Type _) : Bool → Type _ where
 | vertex : Vertex α -> Path α false -> Path α true
 | edge : Nat -> Path α true -> Path α false
 | empty : Path α false


private def findMinimum (set : Std.HashSet Nat) (distances : Array ((Option Nat) × Nat)) : Nat := 
  let min : Option Nat -> Nat -> Option Nat := fun leftIdOption rightId => match leftIdOption with 
    | none => some rightId
    | some leftId => 
      let leftDistance := distances[leftId].1
      let rightDistance := distances[rightId].1
      match rightDistance with
        | none => some leftId
        | some r => match leftDistance with
          | none => some rightId
          | some l => if l < r then some leftId else some rightId

  match set.fold min none with
    | none => panic! "this should not be possible"
    | some temp => temp


-- Note for thesis :Fuel pattern - give enough fuel to always treminate
private def dijkstraAux (g : Graph α) (current : Nat) (target : Option Nat) (unvisited : Std.HashSet Nat) (distanceAndPredecessor : Array ((Option Nat) × Nat)) : Nat -> Array ((Option Nat) × Nat)
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
    let isTargetFound : Bool := match target with 
      | none => false
      | some t => t == nextCurrent
    if isTargetFound then distances else
      match distances[nextCurrent].1 with
        | none => distances
        | some x => dijkstraAux g nextCurrent target (unvisited.erase nextCurrent) distances n


private def dijkstraAuxBase (g : Graph α) (source : Nat) (target : Option Nat) : Array ((Option Nat) × Nat) :=
  let distanceAndPredecessorInitial : Array ((Option Nat) × Nat) := mkArray g.vertices.size (none, source) -- (weight, parent) pairs initialized to (infinitiy, placeholder)
  if h : source < distanceAndPredecessorInitial.size then
    let distanceAndPredecessor := distanceAndPredecessorInitial.set ⟨source, h⟩ (some 0, source)
    let isTargetFound : Bool := match target with
      | some t => t == source
      | none => false
    if isTargetFound then distanceAndPredecessor
    else
      let unvisitedSet : Std.HashSet Nat := do
        let mut temp : Std.HashSet Nat := Std.HashSet.empty
        for i in [0:g.vertices.size] do temp := temp.insert i
        temp
      dijkstraAux g source target (unvisitedSet.erase source) distanceAndPredecessor (unvisitedSet.size-1)
  else 
      panic! "source out of bounds"


-- TODO create wrapper for array opt nat nat with funtcions to return specific paths from the tree or the whole tree
def dijkstraUnsafe (g : Graph α) (source : Nat) : Array ((Option Nat) × Nat) := dijkstraAuxBase g source none

def dijkstraUnsafeWithDestination (g : Graph α) (source : Nat) (target : Nat) : Array ((Option Nat) × Nat) := dijkstraAuxBase g source (some target)

-- def dijkstraSafe TODO

end Graph