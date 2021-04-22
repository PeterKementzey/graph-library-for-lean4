import Graph.graphrepresentation
import Std.Data.Queue
import Std.Data.Stack

namespace Graph

variable {α : Type} [BEq α] [Inhabited α]

#check Std.Queue

universe u

private structure Container (β : Type u) (containerType : Type u -> Type u) where
  χ := containerType β
  container : χ
  addFun :χ -> β -> χ
  removeFun : χ -> χ
  get : χ -> β
  -- getFun : χ -> β

namespace Container

def add (cont : Container b c) (x : b) : Container b c := {
  cont with container := cont.addFun cont.container x
}

def remove (cont : Container b c) : Container b c := {
  cont with container := cont.removeFun cont.container
}

-- def get (cont : Container b c) : b :=
--   cont.getFun cont.container


#check Std.Stack

private def testing : Nat -> List Nat := do
  let mut container : Container Nat Std.Stack := { container := Std.Stack.empty, addFun := Std.Stack.push, removeFun := Std.Stack.pop, get := Std.Stack.peek! }
  container := container.add 3

  _



end Container

-- TODO make it generic stack - queue (tip: bundle them into a structure which is either stack or queue)
private def BFSAux (g : Graph α) (target : Nat) (visited : Array Bool) (q : Std.Queue Nat) : Nat -> Bool
  | 0 => false
  | n + 1 => do
    let mut queue : Std.Queue Nat := q
    let mut visitedMutable := visited
    match queue.dequeue? with
      | none => return false
      | some x =>
        let current := x.1
        queue := x.2
        for edge in g.vertices[current].adjacencyList do
          if !visited[edge.target] then
            if edge.target == target then return true
            visitedMutable := visitedMutable.set! edge.target true
            queue := queue.enqueue edge.target
        BFSAux g target visitedMutable queue n
    

def breadthFirstSearch (g : Graph α) (source : Nat) (target : Nat) : Bool := 
  if source == target then true
  else 
    let visited : Array Bool := mkArray g.vertices.size false
    let q : Std.Queue Nat := Std.Queue.empty
    BFSAux g target (visited.set! source true) (q.enqueue source) g.vertices.size

end Graph