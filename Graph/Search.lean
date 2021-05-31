import Graph.Traverse

namespace Graph

variable {α : Type} [BEq α] [Inhabited α] variable {β : Type}

private def searchVisit (target : Nat) (id : Nat) (state : Bool) :=
  if id == target then (true, true)
  else (false, false)

def depthFirstSearch (g : Graph α β) (source : Nat) (target : Nat) : Bool := g.depthFirstTraversal source false (searchVisit target)

def breadthFirstSearch (g : Graph α β) (source : Nat) (target : Nat) : Bool := g.breadthFirstTraversal source false (searchVisit target)

namespace UndirectedGraph

def breadthFirstSearch (ug : UndirectedGraph α β) := ug.graph.breadthFirstSearch

def depthFirstSearch (ug : UndirectedGraph α β) := ug.graph.depthFirstSearch

end UndirectedGraph
end Graph
