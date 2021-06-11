import Graph.Graph

namespace Graph

private def indexArray (size : Nat) : Array Nat := do
  let mut arr : Array Nat := mkArray size 0
  for i in [0:size] do
    arr := arr.set! i i
  arr

variable {α : Type} [Inhabited α] {β : Type}

inductive Tree (g : Graph α β) where
  | expandedTree : Nat -> Array (Tree g) -> Tree g
  | nonExpandedTree : Nat -> Tree g

namespace Tree

def expandTree (t : Tree (g : Graph α β)) (visited : Array Bool) : Tree g := match t with
  | expandedTree _ _ => t
  | nonExpandedTree node => 
      let adjacencyList := (g.vertices[node].adjacencyList.map (λ edge => edge.target)).filter (!visited[.])
      let subForest := adjacencyList.map (λ n => nonExpandedTree n)
      expandedTree node subForest

end Tree

-- def generateTree (g : Graph α β) (node : Nat) : Tree := 


end Graph