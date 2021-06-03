import Graph.Traverse

namespace Graph

variable {α : Type} [Inhabited α] {β : Type}

private def searchVisit (target : Nat) (id : Nat) (state : Bool) :=
  if id == target then (true, true)
  else (false, false)

/-- Depth-first search of graph. Returns true if target node is reachable from source node. -/
def depthFirstSearch (g : Graph α β) (source : Nat) (target : Nat) : Bool := g.depthFirstTraversal source false (searchVisit target)

/-- Breadt-first search of graph. Returns true if target node is reachable from source node. -/
def breadthFirstSearch (g : Graph α β) (source : Nat) (target : Nat) : Bool := g.breadthFirstTraversal source false (searchVisit target)

namespace UndirectedGraph

/-- Depth-first search of graph. Returns true if target node is reachable from source node. -/
def depthFirstSearch (ug : UndirectedGraph α β) := ug.graph.depthFirstSearch

/-- Breadth-first search of graph. Returns true if target node is reachable from source node. -/
def breadthFirstSearch (ug : UndirectedGraph α β) := ug.graph.breadthFirstSearch

end UndirectedGraph
end Graph
