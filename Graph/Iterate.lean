import Graph.Graph
import Std.Data.Queue
import Std.Data.Stack

import Graph.Search

namespace Graph

open Internal

variable {α : Type} [BEq α] [Inhabited α]

private def iterateAux {β : Type _} (g : Graph α) (visited : Array Bool) (container : Container Nat χ) (visit : Nat -> β -> β × Bool) (default : β) : Nat -> β
  | 0 => default -- The iteration has not terminated on any node, might change default to a function which calculates the return value based on the current state (β)
  | n+1 => match container.remove? with
    | none => default -- This case is theoretically impossible to happen
    | some (currentNodeId, containerWithNodeRemoved) => do
      let (res, terminate?) := visit currentNodeId false
      match terminate? with
        | true => return res
        | false =>
          let unvisitedNeighborIds := (g.vertices[currentNodeId].adjacencyList.map (λ e => e.target)).filter (λ e => !visited[e])
          let mut visitedMut := visited -- Can I avoid using this with some functional magic?
          for neighbor in unvisitedNeighborIds do
            visitedMut := visitedMut.set! neighbor true
          let containerWithNewNodes := containerWithNodeRemoved.addAll unvisitedNeighborIds
          iterateAux g visitedMut containerWithNewNodes visit default n


-- def breadthFirstSearch (g : Graph α) (source : Nat) (target : Nat) : Bool :=
--   if source == target then true
--   else
--     let visited : Array Bool := mkArray g.vertices.size false
--     iterateAux g target (visited.set! source true) (Container.emptyQueue.add source) g.vertices.size


-- def depthFirstIteration (g : Graph α) (source : Nat) (visit : Graph α -> Nat -> Bool × Bool) (default : Bool) : Bool :=
--   let visited : Array Bool := mkArray g.vertices.size false
--   iterateAux g (visited.set! source true) (Container.emptyStack.add source) visit default g.vertices.size

-- def searchVisit (target : Nat) (g : Graph α) (id : Nat) :=
--   if id == target then (true, true)
--   else (false, false)

-- def depthFirstSearch2 (g : Graph α) (source : Nat) (target : Nat) : Bool := depthFirstIteration g source (searchVisit target) false

end Graph
