import Graph.graphrepresentation
import Std.Data.Queue

namespace Graph

variable {α : Type} [BEq α] [Inhabited α]

def BFSAux (g : Graph α) (target : Nat) (visited : Array Bool) (q : Std.Queue Nat) : Nat -> Bool
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