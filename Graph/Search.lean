import Graph.Iterate

namespace Graph

variable {α : Type} [BEq α] [Inhabited α]

def searchVisit (target : Nat) (id : Nat) (state : Bool) :=
  if id == target then (true, true)
  else (false, false)

def depthFirstSearch (g : Graph α) (source : Nat) (target : Nat) : Bool := g.depthFirstIteration source false (searchVisit target) none

def breadthFirstSearch (g : Graph α) (source : Nat) (target : Nat) : Bool := g.breadthFirstIteration source false (searchVisit target) none

end Graph
