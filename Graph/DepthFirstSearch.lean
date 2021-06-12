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

private partial def createForestAux (t : Tree (g : Graph α β)) (pair : Array (Tree g) × (Array Bool)) : Array (Tree g) × (Array Bool) :=
  let forest := pair.1
  let visited := pair.2
  match t with
    | expandedNode _ _ => panic! "Expandind expanded node"
    | nonExpandedNode id =>
        if visited[id] then pair else
        let visited := visited.set! id true
        let adjacencyList := (g.vertices[id].adjacencyList.map (λ edge => edge.target)).filter (!visited[.])
        let subForest : Array (Tree g) := adjacencyList.map (λ n => nonExpandedNode n)
        let (subForest, visited) := subForest.foldr createForestAux (#[], visited)
        let forest := forest.push (expandedNode id subForest)
        (forest, visited)

def fullyExpand (t : Tree (g : Graph α β)) : Tree g := (t.createForestAux (#[], mkArray g.vertices.size false)).1.back

end Tree
open Tree

def createForest (sources : Array Nat) (g : Graph α β) : Array (Tree g) :=
  let nodes : Array (Tree g) := sources.map (λ id => nonExpandedNode id)
  let visited := mkArray g.vertices.size false
  let (forest, visited) := nodes.foldr createForestAux (#[], visited)
  forest

def createSpanningForest (g : Graph α β) := g.createForest g.getAllVertexIDs

private partial def depthFirstTraversalAux (visit : Nat -> γ -> γ × Bool) (leave : (Nat -> γ -> γ )) (t : Tree (g : Graph α β)) (state : γ ) : γ :=
  have : Inhabited γ := ⟨ state ⟩
  match t with
    | nonExpandedNode _ => panic! "Contains non expanded node!"
    | expandedNode id subForest =>
      let (state, terminate?) := visit id state
      let state := if !terminate? then subForest.foldr (depthFirstTraversalAux visit leave) state else state
      leave id state

def depthFirstTraversal3 (g : Graph α β) (sources : Array Nat) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (leave : Nat -> γ -> γ  := (λ _ x => x)) : γ :=
  let forest := g.createForest sources
  forest.foldr (depthFirstTraversalAux visit leave) startingState




-- -- TODO remove this
-- private partial def depthFirstTraversalOrderAux (t : Tree (g : Graph α β)) (arr : Array Int) : Array Int := match t with
--   | nonExpandedNode _ => panic! "This should not be possible!"
--   | expandedNode id subForest =>
--     (subForest.foldr depthFirstTraversalOrderAux (arr.push id)).push (id * -1)

-- def depthFirstTraversalOrderWithLeaving3 (g : Graph α β) (source : Nat) : Array Int :=
--   let tree : Tree g := Tree.nonExpandedNode source
--   let expanded := tree.fullyExpand
--   expanded.depthFirstTraversalOrderAux #[]

end Graph