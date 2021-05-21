import Graph.Traverse

namespace Graph

variable {α : Type} [BEq α] [Inhabited α]

private def searchVisit (target : Nat) (id : Nat) (state : Bool) :=
  if id == target then (true, true)
  else (false, false)

def depthFirstSearch (g : Graph α) (source : Nat) (target : Nat) : Bool := g.depthFirstTraversal source false (searchVisit target)

def breadthFirstSearch (g : Graph α) (source : Nat) (target : Nat) : Bool := g.breadthFirstTraversal source false (searchVisit target)

end Graph
