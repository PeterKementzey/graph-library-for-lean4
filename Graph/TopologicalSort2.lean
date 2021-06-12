import Graph.Graph
import Graph.DepthFirstSearch

namespace Graph

variable {α : Type} [Inhabited α] {β : Type}

private structure State where
  temporaryMark : Array Bool
  permanentMark : Array Bool
  res : Array Nat

private instance : Inhabited State := ⟨ arbitrary, arbitrary, arbitrary ⟩
    

/-- Retruns a topological ordering of any DAG (directed acyclic graph). If the graph contains cycles then returns none. -/
def topSort2 (g : Graph α β) : Option (Array Nat) :=
  let res : Option State := g.depthFirstCompleteTraversal3 initialState (visit g) leave
  match res with
    | some state => some state.res.reverse
    | none => none
  where
    initialState := some ⟨ falseArray, falseArray, Array.empty ⟩
    falseArray := (mkArray g.vertices.size false)

    visit (g : Graph α β) (id : Nat) (s : Option State) : Option State × Bool :=
      let state := s.get!
      if state.permanentMark[id] then return (s, false) else
      if g.vertices[id].adjacencyList.any (λ edge => state.temporaryMark[edge.target]) then return (none, true) else
      let updatedState := { state with
        temporaryMark := state.temporaryMark.set! id true
      }
      (some updatedState, false)

    leave (id : Nat) (s : Option State) : Option State :=
      match s with
        | some state =>
          some { state with
            temporaryMark := state.temporaryMark.set! id false
            permanentMark := state.permanentMark.set! id true
            res := state.res.push id
          }
        | none => none

end Graph