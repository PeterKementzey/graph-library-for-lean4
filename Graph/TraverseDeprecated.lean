import Graph.Graph
import Graph.UndirectedGraph
import Std.Data.Queue
import Std.Data.Stack
import Std.Data.HashSet

import Graph.Container

namespace Graph

open Internal

private def removeDuplicates [BEq α] [Hashable α] (a : Array α) : Array α := do
  let mut set : Std.HashSet α := Std.HashSet.empty
  for e in a do
    set := set.insert e
  set.toArray

variable {α : Type} [Inhabited α] {β : Type}

private def traverseAux {γ : Type _} {containerType : Type _} (g : Graph α β) (visited : Array Bool) (container : Container (Nat × Bool) containerType) (state : γ ) (visit : Nat -> γ -> γ × Bool) (leave : Option (Nat -> γ -> γ )) : Nat -> γ
  | 0 => state
  | n + 1 => match container.remove? with
    | none => state
    | some ((currentNodeId, isArriving), containerWithNodeRemoved) =>
      if visited[currentNodeId] && isArriving then traverseAux g visited containerWithNodeRemoved state visit leave n else
      if isArriving then do
        let (newState, terminate?) := visit currentNodeId state
        match terminate? with
          | true => return newState
          | false =>
            let containerWithLeavingNodeAdded := if leave.isSome then containerWithNodeRemoved.add (currentNodeId, false) else containerWithNodeRemoved
            let visited := visited.set! currentNodeId true
            let unvisitedNeighborIds := removeDuplicates ((g.vertices[currentNodeId].adjacencyList.map (λ e => e.target)).filter (λ e => !visited[e]))
            let unvisitedNeighborIdsWithArrivingFlags := unvisitedNeighborIds.map (λ id => (id, true))
            let containerWithNewNodes := containerWithLeavingNodeAdded.addAll unvisitedNeighborIdsWithArrivingFlags
            traverseAux g visited containerWithNewNodes newState visit leave n
      else
        match leave with
          | some leaveFunction => traverseAux g visited containerWithNodeRemoved (leaveFunction currentNodeId state) visit leave n
          | none => traverseAux g visited containerWithNodeRemoved state visit leave n -- This should be impossible

private def traverseAuxBase {containerType : Type _} (g : Graph α β) (container : Container (Nat × Bool) containerType) (source : Nat) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (leave : Option (Nat -> γ -> γ )) (alreadyVisited : Option (Array Bool)) : γ :=
  let visited : Array Bool := match alreadyVisited with
    | some v => v
    | none => mkArray g.vertexCount false
  traverseAux g visited (container.add (source, true)) startingState visit leave (g.edgeCount + g.vertexCount)

/-- A breadth-first traversal of the graph starting at source. Visit is a function executed at each vertex, its parameters are the vertex ID and the current state,
    it returns a new state and a boolean which terminates the traversal if true. Please provide a starting state. You may optionally provide an array of booleans of length
    |V|, then at each index that is set to true, the vertex with the respective ID and it's successors will be skipped.
    See example uses in Graph/TraverseExample.lean -/
def breadthFirstTraverseDeprecated (g : Graph α β) (source : Nat) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (alreadyVisited : Option (Array Bool) := none) : γ :=
  traverseAuxBase g Container.emptyQueue source startingState visit none alreadyVisited

/-- A depth-first traversal of the graph starting at source. Visit is a function executed at each vertex, its parameters are the vertex ID and the current state,
    it returns a new state and a boolean which terminates the traversal if true. Leave is executed when the node is left, when all its successors have been visited, uses the same state.
    You may optionally provide an array of booleans of length |V|, then at each index that is set to true, the vertex with the respective ID and it's successors will be skipped.
    Please provide a starting state. See example uses in Graph/TraverseExample.lean -/
def depthFirstTraverseDeprecated (g : Graph α β) (source : Nat) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (leave : Option (Nat -> γ -> γ ) := none) (alreadyVisited : Option (Array Bool) := none) : γ :=
  traverseAuxBase g Container.emptyStack source startingState visit leave alreadyVisited


private def traverseAux2 {containerType : Type _} (g : Graph α β) (startingContainer : Container (Nat × Bool) containerType) (startingSources : Array Nat) (startingState : γ) (visit : Nat -> γ -> γ × Bool) (leave : Option (Nat -> γ -> γ)) : γ := do
  let mut visited : Array Bool := mkArray g.vertexCount false
  let mut visitedCount : Nat := 0
  let mut state := startingState
  let mut container := startingContainer
  let mut sources := startingSources
  for i in [0:g.vertexCount+g.edgeCount] do
    if visitedCount == g.vertexCount then break else
    match container.remove? with
      | none =>
        if sources.isEmpty then break else
        container := container.add (sources.back, true)
        sources := sources.pop
      | some ((currentNodeId, true), newContainer) =>
        container := newContainer
        if visited[currentNodeId] then continue else
        let (newState, terminate?) := visit currentNodeId state
        state := newState
        if terminate? then break else
          container := if leave.isSome then container.add (currentNodeId, false) else container
          visited := visited.set! currentNodeId true
          visitedCount := visitedCount + 1
          let unvisitedNeighborIds := ((g.vertices[currentNodeId].adjacencyList.map (λ e => e.target)).filter (λ e => !visited[e]))
          let unvisitedNeighborIdsWithArrivingFlags := unvisitedNeighborIds.map (λ id => (id, true))
          container := container.addAll unvisitedNeighborIdsWithArrivingFlags
      | some ((currentNodeId, false), newContainer) =>
        container := newContainer
        match leave with
          | some leaveFunction => state := leaveFunction currentNodeId state
          | none => panic! "This should not be possible"

  return state

-- TODO remove one of these
def depthFirstTraverseDeprecated2 (g : Graph α β) (sources : Array Nat) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (leave : Option (Nat -> γ -> γ ) := none) : γ :=
  traverseAux2 g Container.emptyStack sources startingState visit leave

-- namespace UndirectedGraph

-- /-- See directed graph. -/
-- def breadthFirstTraversal (ug : UndirectedGraph α β) (source : Nat) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (alreadyVisited : Option (Array Bool) := none) : γ :=
--   ug.graph.breadthFirstTraversal source startingState visit alreadyVisited

-- /-- See directed graph. -/
-- def depthFirstTraversal (ug : UndirectedGraph α β) (source : Nat) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (leave : Option (Nat -> γ -> γ ) := none) (alreadyVisited : Option (Array Bool) := none) : γ :=
--   ug.graph.depthFirstTraversal source startingState visit leave alreadyVisited

-- end UndirectedGraph
end Graph
