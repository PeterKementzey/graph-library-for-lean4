import Graph.Graph
import Graph.UndirectedGraph
import Std.Data.Queue
import Std.Data.Stack
import Std.Data.HashSet

/-!
## Traverse (old)

*Note that this module in not imported by default through `import Graph`, import it explicitly using `import Graph.TraverseDeprecated`.*

This is a nice implementation of depth- and breadth-first search using shared code and a stack and queue respectively. However, due to the limitations of such an implementation these functions are fairly inefficient, in general you should rather use `Graph.Traverse`.
-/

import Graph.Container

namespace Graph

open Internal

variable {α : Type} [Inhabited α] {β : Type}

private def traverseAux {containerType : Type _} (g : Graph α β) (startingContainer : Container (Nat × Bool) containerType) (startingSources : Array Nat) (startingState : γ) (visit : Nat -> γ -> γ × Bool) (leave : Option (Nat -> γ -> γ)) : γ := do
  let mut visited : Array Bool := mkArray g.vertexCount false
  let mut state := startingState
  let mut container := startingContainer
  let mut sources := startingSources
  let mut terminate? : Bool := false
  for i in [0:maximumIterationCount] do
    match container.remove? with
      | none =>
        if terminate? || sources.isEmpty then break
        container := container.add (sources.back, true)
        sources := sources.pop
      | some ((currentNodeId, true), newContainer) =>
        container := newContainer
        if terminate? || visited[currentNodeId] then continue
        (state, terminate?) := visit currentNodeId state
        if !leave.isSome && terminate? then break
        visited := visited.set! currentNodeId true
        container := if leave.isSome then container.add (currentNodeId, false) else container
        container := if terminate? then container else
          let unvisitedNeighborIds := (g.vertices[currentNodeId].adjacencyList.map (λ e => e.target)).filter (λ e => !visited[e])
          let unvisitedNeighborIdsWithArrivingFlags := unvisitedNeighborIds.map (λ id => (id, true))
          container.addAll unvisitedNeighborIdsWithArrivingFlags
      | some ((currentNodeId, false), newContainer) =>
        container := newContainer
        match leave with
          | some leaveFunction => state := leaveFunction currentNodeId state
          | none => panic! "This should not be possible"

  return state
  where
    maximumIterationCount : Nat := visitingAndLeavingEachVertex + addingAllSourcesToContainer + visitingSameVertexMultipleTimes + goodMeasure
    visitingAndLeavingEachVertex := g.vertexCount * 2
    addingAllSourcesToContainer := startingSources.size
    visitingSameVertexMultipleTimes := g.edgeCount
    goodMeasure := 1

/-- A breadth-first traversal of the graph starting at the sources in order. Nodes on the same "level" of the traversal are visited in order of the edges added.
    Visit is a function executed at each vertex, its parameters are the vertex ID and the current state,
    it returns a new state and a boolean which terminates the traversal if true. Please provide a starting state.
    *Note that this function scales significantly worse than `breadthFirstTraverse`.*
    See example uses in Graph/TraverseExample.lean -/
def breadthFirstTraverseDeprecated (g : Graph α β) (sources : Array Nat) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) : γ :=
  traverseAux g Container.emptyQueue sources.reverse startingState visit none

/-- A depth-first traversal of the graph starting at the sources in order. Nodes on the same "level" of the traversal are visited in order of the edges added.
    Visit is a function executed at each vertex, its parameters are the vertex ID and the current state,
    it returns a new state and a boolean which terminates the traversal if true (but will still leave the already visited nodes).
    Leave is executed when the node is left, when all its successors have been visited, uses the same state. Please provide a starting state.
    *Note that this function scales significantly worse than `dephtFirstTraverse`.*
    See example uses in Graph/TraverseExample.lean -/
def depthFirstTraverseDeprecated (g : Graph α β) (sources : Array Nat) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (leave : Option (Nat -> γ -> γ ) := none) : γ :=
  traverseAux g Container.emptyStack sources.reverse startingState visit leave

namespace UndirectedGraph

/-- See directed graph. -/
def breadthFirstTraverseDeprecated (ug : UndirectedGraph α β) (sources : Array Nat) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) : γ :=
  ug.graph.breadthFirstTraverseDeprecated sources startingState visit

/-- See directed graph. -/
def depthFirstTraverseDeprecated (ug : UndirectedGraph α β) (sources : Array Nat) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (leave : Option (Nat -> γ -> γ ) := none) : γ :=
  ug.graph.depthFirstTraverseDeprecated sources startingState visit leave

end UndirectedGraph
end Graph
