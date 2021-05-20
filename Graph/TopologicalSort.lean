import Graph.Graph

namespace Graph

variable {α : Type} [BEq α] [Inhabited α]

structure State where
  temporaryMark : Array Bool
  permanentMark : Array Bool
  res : Array Nat

-- private def visit (g : Graph α) (id : Nat) (state : State) : Option State :=
--   -- Arrive at node
--   if state.permanentMark[id] then return some state else
--   if state.temporaryMark[id] then return none else

--   let stateWithCurrentTempMarked := { state with
--     temporaryMark := state.temporaryMark.set! id true
--   }

--   do
--     let mut s := stateWithCurrentTempMarked
--     for edge in g.vertices[id].adjacencyList do -- Depth first iteration
--       match visit g edge.target s with
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