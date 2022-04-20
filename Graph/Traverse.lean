import Graph.Graph
import Graph.UndirectedGraph
import Std.Data.HashSet

/-!
## Traverse
-/

namespace Graph

variable {α : Type} [Inhabited α] {β : Type}

private def depthFirstTraverseAux (g : Graph α β) (visit : Nat -> γ -> γ × Bool) (leave : (Nat -> γ -> γ )) (state : γ) (sources : Array Nat) (visited : Array Bool) : Nat -> γ × Bool × Array Bool
  | 0 => (state, true, #[])
  | n + 1 => Id.run do
    let mut visited := visited
    let mut state := state
    for id in sources do
      if visited[id] then continue else
      visited := visited.set! id true
      let (newState, terminate?) := visit id state;
      state := newState
      if terminate? then return (leave id state, true, #[]) else
      let adjacencyList := (g.vertices[id].adjacencyList.map (λ edge => edge.target)).filter (!visited[.])
      let (newState, terminate?, newVisited) := g.depthFirstTraverseAux visit leave state adjacencyList visited n
      visited := newVisited
      state := leave id newState
      if terminate? then return (state, true, #[])

    return (state, false, visited)

/-- A depth-first traversal of the graph starting at `sources`. Nodes on the same "level" of the traversal are visited in order of the edges added.
    `visit` is a function executed at each vertex, its parameters are the vertex ID and the current state, it should return a new state and
    a boolean which terminates the traversal if true (but it will still leave the node). The optional parameter `leave` is executed when the node is left,
    when all its successors have been visited, uses the same state.
    Please provide a starting state. See example uses in `Graph.TraverseExample`. -/
def depthFirstTraverse (g : Graph α β) (sources : Array Nat) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (leave : Nat -> γ -> γ  := (λ _ x => x)) : γ :=
  (g.depthFirstTraverseAux visit leave startingState sources (mkArray g.vertexCount false) (g.vertexCount)).1

/-- A depth-first traversal started from all vertices in order. Each vertex is visited exactly once. See `depthFirstTraverse` for more info. -/
def depthFirstCompleteTraverse (g : Graph α β) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (leave : Nat -> γ -> γ  := (λ _ x => x)) : γ :=
  g.depthFirstTraverse g.getAllVertexIDs startingState visit leave

private def breadthFirstTraverseAux (g : Graph α β) (visit : Nat -> γ -> γ × Bool) (state : γ) (startingSources : Array Nat) (sources : Array Nat) (visited : Array Bool) : Nat -> γ
  | 0 => state
  | n + 1 => Id.run do
    let mut visited := visited
    let mut state := state
    let mut nextSources : Std.HashSet Nat := Std.HashSet.empty
    for id in sources do
      visited := visited.set! id true
      let (newState, terminate?) := visit id state;
      state := newState
      if terminate? then return state else
      let adjacencyList := (g.vertices[id].adjacencyList.map (λ edge => edge.target))
      for targetId in adjacencyList do nextSources := nextSources.insert targetId

    let sourcesArray : Array Nat := nextSources.fold (λ arr id => if visited[id] then arr else arr.push id) #[]
    let startingSources := startingSources.filter (!visited[.])
    match (sourcesArray.isEmpty, startingSources.isEmpty) with
      | (false, _) => g.breadthFirstTraverseAux visit state startingSources sourcesArray visited n
      | (_, false) => g.breadthFirstTraverseAux visit state startingSources.pop #[startingSources.back] visited n
      | (true, true) => state

/-- A breadth-first traversals of the graph starting at the `sources` in order, sources should not contain duplicates. Each vertex is only visited at most once.
    Nodes on the same "level" of the traversal will be visited in random order. If you need the order to be fixed then have a look at `breadthFirstTraverseDeprecated`.
    `visit` is a function executed at each vertex, its parameters are the vertex ID and the current state, it should return a new state and a boolean which terminates
    the traversal if true. Please provide a starting state. `maxDepth` is an optional parameter you can use to limit the depth of the traversal.
    See example uses in `Graph.TraverseExample`. -/
def breadthFirstTraverse (g : Graph α β) (sources : Array Nat) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (maxDepth : Nat := g.vertexCount + sources.size) : γ := Id.run do
  g.breadthFirstTraverseAux visit startingState sources.reverse #[] (mkArray g.vertexCount false) maxDepth

/-- A breadth-first traversal started from all vertices in order. Each vertex is visited exactly once. See `breadthFirstTraverse` for more info. -/
def breadthFirstCompleteTraverse (g : Graph α β) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (maxDepth : Nat := g.vertexCount) : γ :=
  g.breadthFirstTraverse g.getAllVertexIDs startingState visit maxDepth


namespace UndirectedGraph

/-- See directed graph. -/
def depthFirstTraverse (ug : UndirectedGraph α β) (sources : Array Nat) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (leave : Nat -> γ -> γ  := (λ _ x => x)) : γ :=
  ug.graph.depthFirstTraverse sources startingState visit leave

/-- See directed graph. -/
def depthFirstCompleteTraverse (ug : UndirectedGraph α β) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (leave : Nat -> γ -> γ  := (λ _ x => x)) : γ :=
  ug.graph.depthFirstCompleteTraverse startingState visit leave

/-- See directed graph. -/
def breadthFirstTraverse (ug : UndirectedGraph α β) (sources : Array Nat) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (maxDepth : Nat := ug.vertexCount) : γ :=
  ug.graph.breadthFirstTraverse sources startingState visit maxDepth

/-- See directed graph. -/
def breadthFirstCompleteTraverse (ug : UndirectedGraph α β) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (maxDepth : Nat := ug.vertexCount) : γ :=
  ug.graph.breadthFirstCompleteTraverse startingState visit maxDepth

end UndirectedGraph
end Graph
