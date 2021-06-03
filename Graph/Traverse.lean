import Graph.Graph
import Graph.UndirectedGraph
import Std.Data.Queue
import Std.Data.Stack

import Graph.Container

namespace Graph

open Internal

variable {α : Type} [Inhabited α] {β : Type}

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
            let mut visitedMut := visited -- Can I avoid using this with some functional magic? TODO make function that does this
            for neighbor in unvisitedNeighborIds do
              visitedMut := visitedMut.set! neighbor true
            let containerWithNewNodes := containerWithLeavingNodeAdded.addAll unvisitedNeighborIdsWithArrivingFlags
            traverseAux g visitedMut containerWithNewNodes newState visit leave n
      else
        match leave with
          | some leaveFunction => traverseAux g visited containerWithNodeRemoved (leaveFunction currentNodeId state) visit leave n
          | none => traverseAux g visited containerWithNodeRemoved state visit leave n -- This should be impossible

/-- A breadth-first traversal of the graph starting at source. Visit is a function executed at each vertex, its parameters are the vertex ID and the current state,
    it returns a new state and a boolean which terminates the traversal if true. Please provide a starting state. See example uses in Graph/Traverse.lean -/
def breadthFirstTraversal (g : Graph α β) (source : Nat) (startingState : γ ) (visit : Nat -> γ  -> γ  × Bool) : γ  :=
  let visited : Array Bool := mkArray g.vertices.size false
  traverseAux g (visited.set! source true) (Container.emptyQueue.add (source, true)) startingState visit none (g.vertices.size * 2)

/-- A depth-first traversal of the graph starting at source. Visit is a function executed at each vertex, its parameters are the vertex ID and the current state,
    it returns a new state and a boolean which terminates the traversal if true. Leave is executed when the node is left, when all its successors have been visited, uses the same state.
    Please provide a starting state. See example uses in Graph/Traverse.lean -/
def depthFirstTraversal (g : Graph α β) (source : Nat) (startingState : γ ) (visit : Nat -> γ  -> γ  × Bool) (leave : Option (Nat -> γ  -> γ ) := none) : γ  :=
  let visited : Array Bool := mkArray g.vertices.size false
  traverseAux g (visited.set! source true) (Container.emptyStack.add (source, true)) startingState visit leave (g.vertices.size * 2)


-- Example use:
private def traversalArrivingOrderVisit (id : Nat) (state : Array Int) := (state.push id, false)
private def traversalLeavingOrderVisit (id : Nat) (state : Array Int) := state.push (id * -1)

-- Results in an array that contains the node ids in order of visiting, node id * (-1) for order of leaving them
def depthFirstTraversalOrder (g : Graph α β) (source : Nat) : Array Int := g.depthFirstTraversal source Array.empty traversalArrivingOrderVisit
def depthFirstTraversalOrderWithLeaving (g : Graph α β) (source : Nat) : Array Int := g.depthFirstTraversal source Array.empty traversalArrivingOrderVisit (some traversalLeavingOrderVisit)
def breadthFirstTraversalOrder (g : Graph α β) (source : Nat) : Array Int := g.breadthFirstTraversal source Array.empty traversalArrivingOrderVisit

namespace UndirectedGraph

def breadthFirstTraversal (ug : UndirectedGraph α β) (source : Nat) (startingState : γ ) (visit : Nat -> γ  -> γ  × Bool) : γ := ug.graph.breadthFirstTraversal source startingState visit

def depthFirstTraversal (ug : UndirectedGraph α β) (source : Nat) (startingState : γ ) (visit : Nat -> γ  -> γ  × Bool) (leave : Option (Nat -> γ  -> γ ) := none) : γ := ug.graph.depthFirstTraversal source startingState visit leave

end UndirectedGraph
end Graph
