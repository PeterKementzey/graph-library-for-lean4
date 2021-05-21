import Graph.Graph
import Std.Data.Queue
import Std.Data.Stack

import Graph.Container

namespace Graph

open Internal

variable {α : Type} [BEq α] [Inhabited α]

private def iterateAux {β : Type _} {containerType : Type _} (g : Graph α) (visited : Array Bool) (container : Container Nat containerType) (state : β) (visit : Nat -> β -> β × Bool) (leave : Option (Nat -> β -> β)) : Nat -> β
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
          let stateAfterFurtherSearch := iterateAux g visitedMut containerWithNewNodes newState visit leave n
          match leave with
            | some leaveFunction => leaveFunction currentNodeId stateAfterFurtherSearch
            | none => stateAfterFurtherSearch


def breadthFirstIteration (g : Graph α) (source : Nat) (startingState : β) (visit : Nat -> β -> β × Bool) (leave : Option (Nat -> β -> β)) : β :=
  let visited : Array Bool := mkArray g.vertices.size false
  iterateAux g (visited.set! source true) (Container.emptyQueue.add source) startingState visit leave g.vertices.size

def depthFirstIteration (g : Graph α) (source : Nat) (startingState : β) (visit : Nat -> β -> β × Bool) (leave : Option (Nat -> β -> β)) : β :=
  let visited : Array Bool := mkArray g.vertices.size false
  iterateAux g (visited.set! source true) (Container.emptyStack.add source) startingState visit leave g.vertices.size




def depthFirstSearch2 (g : Graph α) (source : Nat) (target : Nat) : Bool := g.depthFirstIteration source false (searchVisit target) none where
  searchVisit (target : Nat) (id : Nat) (state : Bool) :=
    if id == target then (true, true)
    else (false, false)




def iterationOrder (id : Nat) (state : Array Nat) := (state.push id, false)
def iterationReverseOrder (id : Nat) (state : Array Nat) := state.push id

def depthFirstIterationOrder (g : Graph α) (source : Nat) : Array Nat := g.depthFirstIteration source Array.empty iterationOrder (some iterationReverseOrder)
def breadthFirstIterationOrder (g : Graph α) (source : Nat) : Array Nat := g.breadthFirstIteration source Array.empty iterationOrder (some iterationReverseOrder)

end Graph
