import Graph.Graph
import Graph.UndirectedGraph
import Std.Data.HashSet

namespace Graph

variable {α : Type} [Inhabited α] {β : Type}

private def depthFirstTraverseAux (g : Graph α β) (visit : Nat -> γ -> γ × Bool) (leave : (Nat -> γ -> γ )) (state : γ) (sources : Array Nat) (visited : Array Bool) : Nat -> γ × Bool × Array Bool
  | 0 => (state, true, #[])
  | n + 1 => do
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

def depthFirstTraverse (g : Graph α β) (sources : Array Nat) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (leave : Nat -> γ -> γ  := (λ _ x => x)) : γ :=
  (g.depthFirstTraverseAux visit leave startingState sources (mkArray g.vertices.size false) (g.vertices.size)).1

def depthFirstCompleteTraverse (g : Graph α β) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (leave : Nat -> γ -> γ  := (λ _ x => x)) : γ :=
  g.depthFirstTraverse g.getAllVertexIDs startingState visit leave

private def breadthFirstTraverseAux (g : Graph α β) (visit : Nat -> γ -> γ × Bool) (state : γ) (sources : Array Nat) (visited : Array Bool) : Nat -> γ
  | 0 => state
  | n + 1 => do
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
    g.breadthFirstTraverseAux visit state sourcesArray visited n


-- TODO mention that there should be no duplicates here
def breadthFirstTraverse (g : Graph α β) (sources : Array Nat) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (maxDepth : Nat := g.vertices.size) : γ :=
  g.breadthFirstTraverseAux visit startingState sources (mkArray g.vertices.size false) maxDepth

def breadthFirstCompleteTraverse (g : Graph α β) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (maxDepth : Nat := g.vertices.size) : γ :=
  g.breadthFirstTraverse g.getAllVertexIDs startingState visit maxDepth


namespace UndirectedGraph

def depthFirstTraverse (ug : UndirectedGraph α β) (sources : Array Nat) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (leave : Nat -> γ -> γ  := (λ _ x => x)) : γ :=
  ug.graph.depthFirstTraverse sources startingState visit leave

def depthFirstCompleteTraverse (ug : UndirectedGraph α β) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (leave : Nat -> γ -> γ  := (λ _ x => x)) : γ :=
  ug.graph.depthFirstCompleteTraverse startingState visit leave

def breadthFirstTraverse (ug : UndirectedGraph α β) (sources : Array Nat) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (maxDepth : Nat := ug.vertexCount) : γ :=
  ug.graph.breadthFirstTraverse sources startingState visit maxDepth

def breadthFirstCompleteTraverse (ug : UndirectedGraph α β) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (maxDepth : Nat := ug.vertexCount) : γ :=
  ug.graph.breadthFirstCompleteTraverse startingState visit maxDepth

end UndirectedGraph
end Graph