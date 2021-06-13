import Graph.TraverseDeprecated

namespace Graph

variable {α : Type} [Inhabited α] {β : Type}

private def searchVisit (target : Nat) (id : Nat) (state : Bool) :=
  if id == target then (true, true)
  else (false, false)

/-- Depth-first search of graph. Returns true if target node is reachable from source node. -/
def depthFirstSearch (g : Graph α β) (source : Nat) (target : Nat) : Bool := g.depthFirstTraversal source false (searchVisit target)

/-- Breadt-first search of graph. Returns true if target node is reachable from source node. -/
def breadthFirstSearch (g : Graph α β) (source : Nat) (target : Nat) : Bool := g.breadthFirstTraversal source false (searchVisit target)

private def pathVisit (target : Nat) (id : Nat) (state : Array Nat) :=
  if id == target then (state.push id, true)
  else (state.push id, false)

private def pathLeave (id : Nat) (state : Array Nat) := state.pop

private def constructPath (g : Graph α β) (vertexStack : Array Nat) (pathSoFar : Path β true) : Nat -> Path β true
  | 0 => panic! "This should not be possible"
  | n + 1 =>
    let currentId := vertexStack.back
    let edgeWeight := match pathSoFar with
      | Path.empty => none
      | Path.vertex id p => match g.vertices[currentId].adjacencyList.find? (λ edge => edge.target == id) with
        | some edge => some edge.weight
        | none => none
    let pathWithCurrentEdgeAdded : Path β false := match edgeWeight with
      | some weight => Path.edge weight pathSoFar
      | none => Path.empty

    let pathWithCurrentVertexAdded : Path β true := Path.vertex currentId pathWithCurrentEdgeAdded
    let newStack := vertexStack.pop
    if newStack.isEmpty then
      pathWithCurrentVertexAdded
    else
      constructPath g newStack pathWithCurrentVertexAdded n

/-- Returns a Path from source to target if reachable, none otherwise. -/
def depthFirstPathSearch (g : Graph α β) (source : Nat) (target : Nat) : Option (Path β true) :=
  let vertexStack := g.depthFirstTraversal source Array.empty (pathVisit target) (some pathLeave)
  if !vertexStack.isEmpty then
    some (constructPath g vertexStack Path.empty vertexStack.size)
  else
    none

namespace UndirectedGraph

/-- See directed graph. -/
def depthFirstSearch (ug : UndirectedGraph α β) := ug.graph.depthFirstSearch

/-- See directed graph. -/
def breadthFirstSearch (ug : UndirectedGraph α β) := ug.graph.breadthFirstSearch

/-- See directed graph. -/
def depthFirstPathSearch (ug : UndirectedGraph α β) := ug.graph.depthFirstPathSearch

end UndirectedGraph
end Graph
