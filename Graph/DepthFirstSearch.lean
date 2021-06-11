import Graph.Graph

namespace Graph

private def indexArray (size : Nat) : Array Nat := do
  let mut arr : Array Nat := mkArray size 0
  for i in [0:size] do
    arr := arr.set! i i
  arr

variable {α : Type} [Inhabited α] {β : Type}

inductive Tree (g : Graph α β) where
  | expandedNode : Nat -> Array (Tree g) -> Tree g
  | nonExpandedNode : Nat -> Tree g

namespace Tree

instance : Inhabited (Tree (g : Graph α β)) where default := nonExpandedNode arbitrary

def expand (t : Tree (g : Graph α β)) (visited : Array Bool) : Tree g := match t with
  | expandedNode _ _ => t
  | nonExpandedNode id => 
      let adjacencyList := (g.vertices[id].adjacencyList.map (λ edge => edge.target)).filter (!visited[.])
      let subForest := adjacencyList.map (λ n => nonExpandedNode n)
      expandedNode id subForest

private partial def fullyExpandAux (t : Tree (g : Graph α β)) (visited : Array Bool) : (Tree g) × (Array Bool) := match t with
  | expandedNode _ _ => (t, visited)
  | nonExpandedNode id => 
      let adjacencyList := (g.vertices[id].adjacencyList.map (λ edge => edge.target)).filter (!visited[.])
      let subForest : Array (Tree g) := adjacencyList.map (λ n => nonExpandedNode n)
      let (newForest, newVisited) := subForest.foldr aux (#[], visited.set! id true)
      (expandedNode id newForest, newVisited)

      where aux (t : Tree (g : Graph α β)) (pair : (Array (Tree g)) × (Array Bool)) :=
        let forest := pair.1
        let visited := pair.2
        let (newTree, newVisited) := t.fullyExpandAux visited
        (forest.push newTree, newVisited)

def fullyExpand (t : Tree (g : Graph α β)) : Tree g := (t.fullyExpandAux (mkArray g.vertices.size false)).1

partial def depthFirstTraversalOrderAux (t : Tree (g : Graph α β)) (arr : Array Int) : Array Int := match t with
  | nonExpandedNode _ => panic! "This should not be possible!"
  | expandedNode id subForest => (subForest.foldr depthFirstTraversalOrderAux (arr.push id)).push (id * -1)

end Tree

def depthFirstTraversalOrderWithLeaving3 (g : Graph α β) (source : Nat) : Array Int :=
  let tree : Tree g := Tree.nonExpandedNode source
  let expanded := tree.fullyExpand
  expanded.depthFirstTraversalOrderAux #[]

end Graph