import Graph.Graph
import Graph.Traverse

namespace Graph

variable {α : Type} [Inhabited α] {β : Type}

structure State where
  temporaryMark : Array Bool
  permanentMark : Array Bool
  res : Array Nat

instance : Inhabited State := ⟨ arbitrary, arbitrary, arbitrary ⟩

private def topologicalSortAux (g : Graph α β) (s : Option State) : Nat -> Option State
  | 0 => s
  | n + 1 => match s with
    | some state =>
      match state.permanentMark.findIdx? (!.) with
        | some unmarkedId =>
          let stateWithConnectedComponentSorted := g.depthFirstTraversal unmarkedId s (visit g) leave (some state.permanentMark)
          g.topologicalSortAux stateWithConnectedComponentSorted n
        | none => state
    | none => none

  where
    visit (g : Graph α β) (id : Nat) (s : Option State) : Option State × Bool :=
      let state := s.get!
      if state.permanentMark[id] then return (s, false) else
      if g.vertices[id].adjacencyList.any (λ edge => state.temporaryMark[edge.target]) then return (none, true) else
      let updatedState := { state with
        temporaryMark := state.temporaryMark.set! id true
      }
      (some updatedState, false)

    leave (id : Nat) (s : Option State) : Option State :=
      let state := s.get!
      some { state with
        temporaryMark := state.temporaryMark.set! id false
        permanentMark := state.permanentMark.set! id true
        res := state.res.push id
      }

/-- Retruns a topological ordering of any DAG (directed acyclic graph). If the graph contains cycles then returns none. -/
def topSort (g : Graph α β) : Option (Array Nat) :=
  let res : Option State := g.topologicalSortAux initialState g.vertices.size
  match res with
    | some state => some state.res.reverse
    | none => none
  where
    initialState := some ⟨ falseArray, falseArray, Array.empty ⟩
    falseArray := (mkArray g.vertices.size false)

end Graph