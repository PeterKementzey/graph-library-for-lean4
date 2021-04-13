import Graph.graphrepresentation
import Std.Data.Queue

namespace Graph

variable {α : Type} [BEq α] [Inhabited α]

def DFSAux (g : Graph α) (target : Nat) (visited : Array Bool) (q : Std.Queue Nat) : Nat -> Bool
  | 0 => false
  | n + 1 => true


def depthFirstSearch (g : Graph α) (source : Nat) (target : Nat) : Bool := 
  let visited : Array Bool := mkArray g.vertices.size false
  let q : Std.Queue Nat := Std.Queue.empty
  DFSAux g target visited (q.enqueue source) g.vertices.size

end Graph