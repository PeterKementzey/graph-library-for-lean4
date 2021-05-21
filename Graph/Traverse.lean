import Graph.Graph
import Std.Data.Queue
import Std.Data.Stack

import Graph.Container

namespace Graph

open Internal

variable {α : Type} [BEq α] [Inhabited α]

private def traverseAux {β : Type _} {containerType : Type _} (g : Graph α) (visited : Array Bool) (container : Container Nat containerType) (state : β) (visit : Nat -> β -> β × Bool) (leave : Option (Nat -> β -> β)) : Nat -> β
  | 0 => state
  | n + 1 => match container.remove? with
    | none => state
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
          let stateAfterFurtherSearch := traverseAux g visitedMut containerWithNewNodes newState visit leave n
          match leave with
            | some leaveFunction => leaveFunction currentNodeId stateAfterFurtherSearch
            | none => stateAfterFurtherSearch


def breadthFirstTraversal (g : Graph α) (source : Nat) (startingState : β) (visit : Nat -> β -> β × Bool) (leave : Option (Nat -> β -> β) := none) : β :=
  let visited : Array Bool := mkArray g.vertices.size false
  traverseAux g (visited.set! source true) (Container.emptyQueue.add source) startingState visit leave g.vertices.size

def depthFirstTraversal (g : Graph α) (source : Nat) (startingState : β) (visit : Nat -> β -> β × Bool) (leave : Option (Nat -> β -> β) := none) : β :=
  let visited : Array Bool := mkArray g.vertices.size false
  traverseAux g (visited.set! source true) (Container.emptyStack.add source) startingState visit leave g.vertices.size


-- Example use:
private def traversalOrder (id : Nat) (state : Array Nat) := (state.push id, false)
private def traversalReverseOrder (id : Nat) (state : Array Nat) := state.push id

-- Results in an array that contains the node ids in order of visiting, then in reverse order (added when leaving the node)
def depthFirstTraversalOrder (g : Graph α) (source : Nat) : Array Nat := g.depthFirstTraversal source Array.empty traversalOrder
def breadthFirstTraversalOrder (g : Graph α) (source : Nat) : Array Nat := g.breadthFirstTraversal source Array.empty traversalOrder

end Graph
