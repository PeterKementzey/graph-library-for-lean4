import Graph.Graph
import Graph.Traverse

/-!
## Topological Sorting
-/

namespace Graph

variable {α : Type} [Inhabited α] {β : Type}

private structure State where
  temporaryMark : Array Bool
  res : Array Nat

private instance : Inhabited State := ⟨ default, default ⟩

/-- Retruns a topological ordering of any DAG (directed acyclic graph). If the graph contains cycles then returns none. If you know your graph is a DAG you can use topSortUnsafe for better performance. -/
def topSort (g : Graph α β) : Option (Array Nat) :=
  let res : Option State := g.depthFirstTraverse g.getAllVertexIDs initialState (visit g) leave
  match res with
    | some state => some state.res.reverse
    | none => none
  where
    initialState := some ⟨ falseArray, Array.empty ⟩
    falseArray := (mkArray g.vertexCount false)

    visit (g : Graph α β) (id : Nat) (s : Option State) : Option State × Bool :=
      let state := s.get!
      if g.vertices[id].adjacencyList.any (λ edge => state.temporaryMark[edge.target]) then (none, true) else
      let updatedState := { state with
        temporaryMark := state.temporaryMark.set! id true
      }
      (some updatedState, false)

    leave (id : Nat) (s : Option State) : Option State :=
      match s with
        | some state =>
          some { state with
            temporaryMark := state.temporaryMark.set! id false
            res := state.res.push id
          }
        | none => none

/-- Retruns a topological ordering of any DAG (directed acyclic graph). This function is faster than topSort but if the graph contains cycles then returns wrong results. -/
def topSortUnsafe (g : Graph α β) : Array Nat :=
  let res := g.depthFirstTraverse g.getAllVertexIDs initialState (visit g) leave
  res.reverse
  where
    initialState := #[]

    visit (g : Graph α β) (id : Nat) (s : Array Nat) : Array Nat × Bool := (s, false)

    leave (id : Nat) (s : Array Nat) : Array Nat := s.push id

end Graph
