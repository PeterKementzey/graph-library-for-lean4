import Graph.Graph
import Std.Data.Queue
import Std.Data.Stack

import Graph.Container

namespace Graph

open Internal

variable {α : Type} [BEq α] [Inhabited α] variable {β : Type}

private def traverseAux {γ  : Type _} {containerType : Type _} (g : Graph α β) (visited : Array Bool) (container : Container (Nat × Bool) containerType) (state : γ ) (visit : Nat -> γ  -> γ  × Bool) (leave : Option (Nat -> γ  -> γ )) : Nat -> γ
  | 0 => state
  | n + 1 => match container.remove? with
    | none => state
    | some ((currentNodeId, isArriving), containerWithNodeRemoved) =>
      if isArriving then do
        let (newState, terminate?) := visit currentNodeId state
        match terminate? with
          | true => return newState
          | false =>
            let containerWithLeavingNodeAdded := if leave.isSome then containerWithNodeRemoved.add (currentNodeId, false) else containerWithNodeRemoved
            let unvisitedNeighborIds := (g.vertices[currentNodeId].adjacencyList.map (λ e => e.target)).filter (λ e => !visited[e])
            let unvisitedNeighborIdsWithArrivingFlags := unvisitedNeighborIds.map (λ id => (id, true))
            let mut visitedMut := visited -- Can I avoid using this with some functional magic?
            for neighbor in unvisitedNeighborIds do
              visitedMut := visitedMut.set! neighbor true
            let containerWithNewNodes := containerWithLeavingNodeAdded.addAll unvisitedNeighborIdsWithArrivingFlags
            traverseAux g visitedMut containerWithNewNodes newState visit leave n
      else
        match leave with
          | some leaveFunction => traverseAux g visited containerWithNodeRemoved (leaveFunction currentNodeId state) visit leave n
          | none => traverseAux g visited containerWithNodeRemoved state visit leave n -- This should be impossible


def breadthFirstTraversal (g : Graph α β) (source : Nat) (startingState : γ ) (visit : Nat -> γ  -> γ  × Bool) (leave : Option (Nat -> γ  -> γ ) := none) : γ  :=
  let visited : Array Bool := mkArray g.vertices.size false
  traverseAux g (visited.set! source true) (Container.emptyQueue.add (source, true)) startingState visit leave (g.vertices.size * 2)

def depthFirstTraversal (g : Graph α β) (source : Nat) (startingState : γ ) (visit : Nat -> γ  -> γ  × Bool) (leave : Option (Nat -> γ  -> γ ) := none) : γ  :=
  let visited : Array Bool := mkArray g.vertices.size false
  traverseAux g (visited.set! source true) (Container.emptyStack.add (source, true)) startingState visit leave (g.vertices.size * 2)


-- Example use:
private def traversalArrivingOrderVisit (id : Nat) (state : Array Int) := (state.push id, false)
private def traversalLeavingOrderVisit (id : Nat) (state : Array Int) := state.push (id * -1)

-- Results in an array that contains the node ids in order of visiting, node id * (-1) for order of leaving them
def depthFirstTraversalOrder (g : Graph α β) (source : Nat) : Array Int := g.depthFirstTraversal source Array.empty traversalArrivingOrderVisit
def depthFirstTraversalOrderWithLeaving (g : Graph α β) (source : Nat) : Array Int := g.depthFirstTraversal source Array.empty traversalArrivingOrderVisit traversalLeavingOrderVisit -- Why does this work? Shouldn't it be (some t...traversalLeavingOrderVisit)
def breadthFirstTraversalOrder (g : Graph α β) (source : Nat) : Array Int := g.breadthFirstTraversal source Array.empty traversalArrivingOrderVisit

end Graph
