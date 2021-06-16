import Graph.Graph
import Graph.UndirectedGraph
import Std.Data.Queue
import Std.Data.Stack
import Std.Data.HashSet

/-!
## Traverse (old)

*Note that this module in not imported by default through `import Graph`, import it explicitly using `import Graph.TraverseDeprecated`.*

This is a nice implementation of depth- and breadth-first search using shared code and a stack and queue respectively. However, due to the limitations of such an implementation these functions are fairly inefficient.
-/

import Graph.Container

namespace Graph

open Internal

variable {α : Type} [Inhabited α] {β : Type}

private partial def traverseAuxRec {containerType : Type _} [ToString containerType] (g : Graph α β) (terminate? : Bool) (visited : Array Bool)
  (container : Container (Nat × Bool) containerType) (sources : Array Nat) (state : γ) (visit : Nat -> γ -> γ × Bool) (leave : Option (Nat -> γ -> γ)) : Nat -> γ
  | 0 => have : Inhabited γ := ⟨ state ⟩; panic! "Error: Not enough iterations."
  | n + 1 => 
    match container.remove? with
      | none =>
        if terminate? || sources.isEmpty then state else
        let container := container.add (sources.back, true)
        let sources := sources.pop
        traverseAuxRec g terminate? visited container sources state visit leave n
      | some ((currentNodeId, true), newContainer) =>
        let container := newContainer
        if terminate? || visited[currentNodeId] then traverseAuxRec g terminate? visited container sources state visit leave n else
        let (state, terminate?) := visit currentNodeId state
        if !leave.isSome && terminate? then state else
        let visited := visited.set! currentNodeId true
        let container := if leave.isSome then container.add (currentNodeId, false) else container
        let container := if terminate? then container else
          let unvisitedNeighborIds := (g.vertices[currentNodeId].adjacencyList.map (λ e => e.target)).filter (λ e => !visited[e])
          let unvisitedNeighborIdsWithArrivingFlags := unvisitedNeighborIds.map (λ id => (id, true))
          container.addAll unvisitedNeighborIdsWithArrivingFlags
        traverseAuxRec g terminate? visited container sources state visit leave n
      | some ((currentNodeId, false), newContainer) =>
        let container := newContainer
        match leave with
          | some leaveFunction =>
            let state := leaveFunction currentNodeId state
            traverseAuxRec g terminate? visited container sources state visit leave n
          | none =>
            have : Inhabited γ := ⟨ state ⟩
            panic! "This should not be possible"

def depthFirstTraverseDeprecatedRecursive (g : Graph α β) (sources : Array Nat) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (leave : Option (Nat -> γ -> γ ) := none) : γ :=
  traverseAuxRec g false (mkArray g.vertexCount false) Container.emptyStack sources.reverse startingState visit leave maximumIterationCount
  where
    maximumIterationCount : Nat := visitingAndLeavingEachVertex + addingAllSourcesToContainer + visitingSameVertexMultipleTimes + goodMeasure
    visitingAndLeavingEachVertex := g.vertexCount * 2
    addingAllSourcesToContainer := sources.size
    visitingSameVertexMultipleTimes := g.edgeCount
    goodMeasure := 1

private def traverseAuxDSL {containerType : Type _} (g : Graph α β) (startingContainer : Container (Nat × Bool) containerType) (startingSources : Array Nat) (startingState : γ) (visit : Nat -> γ -> γ × Bool) (leave : Option (Nat -> γ -> γ)) : γ := do
  let mut visited : Array Bool := mkArray g.vertexCount false
  let mut state := startingState
  let mut container := startingContainer
  let mut sources := startingSources
  let mut terminate? : Bool := false
  for i in [0:maximumIterationCount] do
    match container.remove? with
      | none =>
        if terminate? || sources.isEmpty then break else
        container := container.add (sources.back, true)
        sources := sources.pop
      | some ((currentNodeId, true), newContainer) =>
        container := newContainer
        if terminate? || visited[currentNodeId] then continue else
        (state, terminate?) := visit currentNodeId state
        if !leave.isSome && terminate? then break else
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

/-- A breadth-first traversal of the graph starting at the sources in order. Visit is a function executed at each vertex, its parameters are the vertex ID and the current state,
    it returns a new state and a boolean which terminates the traversal if true. Please provide a starting state.
    See example uses in Graph/TraverseExample.lean -/
def breadthFirstTraverseDeprecated (g : Graph α β) (sources : Array Nat) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) : γ :=
  traverseAuxDSL g Container.emptyQueue sources startingState visit none

/-- A depth-first traversal of the graph starting at the sources in order. Visit is a function executed at each vertex, its parameters are the vertex ID and the current state,
    it returns a new state and a boolean which terminates the traversal if true (but will still leave the already visited nodes).
    Leave is executed when the node is left, when all its successors have been visited, uses the same state.
    Please provide a starting state. See example uses in Graph/TraverseExample.lean -/
def depthFirstTraverseDeprecated (g : Graph α β) (sources : Array Nat) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (leave : Option (Nat -> γ -> γ ) := none) : γ :=
  traverseAuxDSL g Container.emptyStack sources.reverse startingState visit leave

-- namespace UndirectedGraph

-- /-- See directed graph. -/
-- def breadthFirstTraversal (ug : UndirectedGraph α β) (source : Nat) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (alreadyVisited : Option (Array Bool) := none) : γ :=
--   ug.graph.breadthFirstTraversal source startingState visit alreadyVisited

-- /-- See directed graph. -/
-- def depthFirstTraversal (ug : UndirectedGraph α β) (source : Nat) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (leave : Option (Nat -> γ -> γ ) := none) (alreadyVisited : Option (Array Bool) := none) : γ :=
--   ug.graph.depthFirstTraversal source startingState visit leave alreadyVisited

-- end UndirectedGraph
end Graph
