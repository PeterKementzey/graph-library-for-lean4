import Graph.Graph
import Std.Data.Queue
import Std.Data.Stack

import Graph.Search

namespace Graph

open Internal

variable {α : Type} [BEq α] [Inhabited α]

private def iterateAux {β : Type _} {containerType : Type _} (g : Graph α) (visited : Array Bool) (container : Container Nat containerType) (visit : Nat -> β -> β × Bool) (state : β) : Nat -> β
  | 0 => state -- The iteration has not terminated on any node, might change to a function which calculates the return value based on the current state (β)
  | n+1 => match container.remove? with
    | none => state -- This case is theoretically impossible to happen
    | some (currentNodeId, containerWithNodeRemoved) => do
      let (newState, terminate?) := visit currentNodeId state
      match terminate? with
        | true => return newState
        | false =>
          let unvisitedNeighborIds := (g.vertices[currentNodeId].adjacencyList.map (λ e => e.target)).filter (λ e => !visited[e])
          let mut visitedMut := visited -- Can I avoid using this with some functional magic?
          for neighbor in unvisitedNeighborIds do
            visitedMut := visitedMut.set! neighbor true
          let containerWithNewNodes := containerWithNodeRemoved.addAll unvisitedNeighborIds
          iterateAux g visitedMut containerWithNewNodes visit newState n


-- def breadthFirstSearch (g : Graph α) (source : Nat) (target : Nat) : Bool :=
--   if source == target then true
--   else
--     let visited : Array Bool := mkArray g.vertices.size false
--     iterateAux g target (visited.set! source true) (Container.emptyQueue.add source) g.vertices.size


def depthFirstIteration (g : Graph α) (source : Nat) (visit : Nat -> β -> β × Bool) (state : β) : β :=
  let visited : Array Bool := mkArray g.vertices.size false
  iterateAux g (visited.set! source true) (Container.emptyStack.add source) visit state g.vertices.size

def searchVisit (target : Nat) (id : Nat) (state : Bool) :=
  if id == target then (true, true)
  else (false, false)

def depthFirstSearch2 (g : Graph α) (source : Nat) (target : Nat) : Bool := g.depthFirstIteration source (searchVisit target) false

end Graph
