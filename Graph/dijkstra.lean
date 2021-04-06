import Graph.graphrepresentation
import Std.Data.HashSet

namespace Graph

-- inductive Path α where
--  | vertex :Vertex α -> Path α -> Path α
--  | edge : Nat -> Path α -> Path α
--  | empty

def dijkstraAux (g: Graph α) (source : Nat) (destination : Nat) (current : Nat) (unvisited : Std.HashSet Nat) (distanceAndPredecessor : Array ((Option Nat) × Nat)) : Array (Nat × Nat) := #[] -- TODO

def dijkstraUnsafe (g : Graph α) (source : Nat) (destination : Nat) : Array (Nat × Nat) := -- Array of (Vertex id, weight of next edge) where weight in last pair is 0
  let distanceAndPredecessorInitial : Array ((Option Nat) × Nat) := mkArray g.vertices.size (none, source) -- (weight, parent) pairs initialized to (infinitiy, placeholder)
  -- let index : Fin (distanceAndPredecessorInitial.size) := source -- Question: how to create Fin type?    (then don't need to use set!, just set)
  let distanceAndPredecessor := distanceAndPredecessorInitial.set! source (some 0, source)
  let unvisitedSet : Std.HashSet Nat := do
    let mut temp : Std.HashSet Nat := Std.HashSet.empty
    for i in [0:g.vertices.size] do temp := temp.insert i
    temp
  dijkstraAux g source destination source (unvisitedSet.erase source) distanceAndPredecessor




-- def dijkstraSafe TODO


end Graph