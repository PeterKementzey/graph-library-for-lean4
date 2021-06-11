import Graph.Traverse

namespace Graph

variable {α : Type} [Inhabited α] {β : Type}

-- Example use:

-- Visit and leave functions
private def traversalArrivingOrderVisit (id : Nat) (state : Array Int) := (state.push id, false)
private def traversalLeavingOrderVisit (id : Nat) (state : Array Int) := state.push (id * -1)

-- Results in an array that contains the node ids in order of visiting
def depthFirstTraversalOrder (g : Graph α β) (source : Nat) : Array Int := g.depthFirstTraversal source Array.empty traversalArrivingOrderVisit

-- Results in an array that contains the node ids in order of visiting, node id * (-1) when leaving them
def depthFirstTraversalOrderWithLeaving (g : Graph α β) (source : Nat) : Array Int := g.depthFirstTraversal source Array.empty traversalArrivingOrderVisit (some traversalLeavingOrderVisit)
def depthFirstTraversalOrderWithLeaving2 (g : Graph α β) (source : Nat) : Array Int := g.depthFirstTraversal2 #[source] Array.empty traversalArrivingOrderVisit (some traversalLeavingOrderVisit)

-- Results in an array that contains the node ids in order of visiting
def breadthFirstTraversalOrder (g : Graph α β) (source : Nat) : Array Int := g.breadthFirstTraversal source Array.empty traversalArrivingOrderVisit

end Graph