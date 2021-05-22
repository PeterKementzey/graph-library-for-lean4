import Graph.Graph
import Graph.Traverse

import Graph.StdExtensions

namespace Graph

variable {α : Type} [BEq α] [Inhabited α]

structure State where
  temporaryMark : Array Bool
  permanentMark : Array Bool
  res : Array Nat

instance : Inhabited State := ⟨ arbitrary, arbitrary, arbitrary ⟩

private def topologicalSortAux (g : Graph α) (s : Option State) : Nat -> Option State
  | 0 => s
  | n + 1 => match s with
    | some state =>
      match state.permanentMark.findIdx? (!.) with
        | some unmarkedId =>
          let stateWithConnectedComponentSorted := g.depthFirstTraversal unmarkedId s (visit g) leave
          g.topologicalSortAux stateWithConnectedComponentSorted n
        | none => state
    | none => none

  where
    visit (g : Graph α) (id : Nat) (s : Option State) : Option State × Bool :=
      let state := s.get!
      if state.permanentMark[id] then return (some state, false) else
      if g.vertices[id].adjacencyList.any (λ edge => state.temporaryMark[edge.target]) then return (none, true) else
      let updatedState := { state with
        temporaryMark := state.temporaryMark.set! id true
      }
      (some updatedState, false)

    leave (id : Nat) (s : Option State) : Option State :=
      let state := s.get!
      some { s.get! with
        temporaryMark := state.temporaryMark.set! id false
        permanentMark := state.permanentMark.set! id true
        res := state.res.push id
      }

def topologicalSort (g : Graph α) : Option (Array Nat) :=
  let res : Option State := g.topologicalSortAux initialState g.vertices.size
  match res with
    | some state => some state.res.reverse
    | none => none
  where
    initialState := some ⟨ (mkArray g.vertices.size false), (mkArray g.vertices.size false), Array.empty ⟩

end Graph