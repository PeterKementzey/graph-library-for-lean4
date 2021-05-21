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

private def visit (id : Nat) (s : Option State) : Option State × Bool :=
  let state := s.get!
  if state.permanentMark[id] then return (some state, false) else
  if state.temporaryMark[id] then return (none, true) else
  
  let updatedState := { state with
    temporaryMark := state.temporaryMark.set! id true
  }

  (some updatedState, false)

private def leave (id : Nat) (s : Option State) : Option State :=
  let state := s.get!
  some { s.get! with
    temporaryMark := state.temporaryMark.set! id false
    permanentMark := state.permanentMark.set! id true
    res := state.res.push id
  }


def topologicalSort (g : Graph α) : Option (Array Nat) := 
  let res : Option State := g.depthFirstTraversal 0 initialState visit (some leave)
  match res with
    | some state => some state.res.reverse
    | none => none
  where
    initialState := some ⟨ (mkArray g.vertices.size false), (mkArray g.vertices.size false), Array.empty ⟩




-- private def visit? (g : Graph α) (id : Nat) (state : State) : Option State :=
--   -- Arrive at node
--   if state.permanentMark[id] then return some state else
--   if state.temporaryMark[id] then return none else

--   let stateWithCurrentTempMarked := { state with
--     temporaryMark := state.temporaryMark.set! id true
--   }

--   do
--     let mut s := stateWithCurrentTempMarked
--     for edge in g.vertices[id].adjacencyList do -- Depth first traversal
--       match visit? g edge.target s with
--         | some x => s := x
--         | none => return none
    
--     -- Leave node
--     let stateWithCurrentPermMarked := { s with
--       temporaryMark := s.temporaryMark.set! id false
--       permanentMark := s.permanentMark.set! id true
--       res := s.res.push id
--     }

--     return some stateWithCurrentPermMarked
  
-- def topologicalSort (g : Graph α) : Array Nat :=
--   -- visit a node without a temporary mark while there are any
--   _

end Graph