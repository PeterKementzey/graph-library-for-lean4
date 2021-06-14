import Graph.Traverse

/-! 
## Traverse example

*Note that this module in not imported by default through `import Graph`, import it explicitly using `import Graph.TraverseExample`.*
    
You can inspect the code in this file to see how the traversal works. -/

namespace Graph

variable {α : Type} [Inhabited α] {β : Type}

-- Example use:

-- Visit and leave functions
private def traversalArrivingOrderVisit (id : Nat) (state : Array Int) := (state.push id, false)
private def traversalLeavingOrderVisit (id : Nat) (state : Array Int) := state.push (id * -1)

-- Results in an array that contains the node ids in order of visiting
def depthFirstTraversalOrder (g : Graph α β) (sources : Array Nat) : Array Int := g.depthFirstTraverse sources Array.empty traversalArrivingOrderVisit
def depthFirstCompleteTraversalOrder (g : Graph α β) : Array Int := g.depthFirstCompleteTraverse Array.empty traversalArrivingOrderVisit

-- Results in an array that contains the node ids in order of visiting, node id * (-1) when leaving them
def depthFirstTraversalOrderWithLeaving (g : Graph α β) (sources : Array Nat) : Array Int := g.depthFirstTraverse sources Array.empty traversalArrivingOrderVisit traversalLeavingOrderVisit
def depthFirstCompleteTraversalOrderWithLeaving (g : Graph α β) : Array Int := g.depthFirstCompleteTraverse Array.empty traversalArrivingOrderVisit traversalLeavingOrderVisit

-- Results in an array that contains the node ids in order of visiting
def breadthFirstTraversalOrder (g : Graph α β) (sources : Array Nat) : Array Int := g.breadthFirstTraverse sources #[] traversalArrivingOrderVisit 
def breadthFirstCompleteTraversalOrder (g : Graph α β) : Array Int := g.breadthFirstCompleteTraverse #[] traversalArrivingOrderVisit 

end Graph