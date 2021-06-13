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

partial def toString (t : Tree (g : Graph α β)) (indent : String := "") : String := match t with
  | nonExpandedNode n => "\n" ++ indent ++ "Non-expanded node: " ++ (ToString.toString n)
  | expandedNode n subForest => "\n" ++ indent ++ "Expanded node: " ++ (ToString.toString n) ++
    if subForest.isEmpty then "" else ((subForest.map (λ node => toString node (indent ++ "  "))).foldl (λ a b => a ++ b) "")

instance : ToString (Tree (g : Graph α β)) where toString t := t.toString

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
    | expandedNode _ _ => panic! "Expanded node"
    | nonExpandedNode id =>
        if visited[id] then pair else
        let visited := visited.set! id true
        let adjacencyList := (g.vertices[id].adjacencyList.map (λ edge => edge.target)).filter (!visited[.])
        let subForest : Array (Tree g) := adjacencyList.map (λ n => nonExpandedNode n)
        let (subForest, visited) := subForest.foldr createForestAux (#[], visited)
        let forest := forest.push (expandedNode id subForest)
        (forest, visited)

private partial def traverseForestAux (visit : Nat -> γ -> γ × Bool) (leave : (Nat -> γ -> γ )) (t : Tree (g : Graph α β)) (pair : γ × (Array Bool)) : γ × (Array Bool) :=
  let state := pair.1
  let visited := pair.2
  have : Inhabited γ := ⟨ state ⟩
  match t with
    | expandedNode _ _ => panic! "Expanded node"
    | nonExpandedNode id =>
        if visited[id] then pair else
        let visited := visited.set! id true
        let (state, terminate?) := visit id state
        -- if terminate? then (leave id state, visited) else
        let adjacencyList := (g.vertices[id].adjacencyList.map (λ edge => edge.target)).filter (!visited[.])
        let subForest : Array (Tree g) := adjacencyList.map (λ n => nonExpandedNode n)
        let (state, visited) := subForest.foldr (traverseForestAux visit leave) (state, visited)
        let state := leave id state
        (state, visited)

    


def fullyExpand (t : Tree (g : Graph α β)) : Tree g := (t.createForestAux (#[], mkArray g.vertices.size false)).1.back -- TODO this might be useless

end Tree

private def depthFirstTraverseAux (g : Graph α β) (visit : Nat -> γ -> γ × Bool) (leave : (Nat -> γ -> γ )) (state : γ) (sources : Array Nat) (visited : Array Bool) : Nat -> γ × Bool × Array Bool
  | 0 => have : Inhabited γ := ⟨ state ⟩; panic! "not enough iterations"
  | n + 1 => do
    let mut visited := visited
    let mut state := state
    for id in sources do
      if visited[id] then continue else
      visited := visited.set! id true
      let (newState, terminate?) := visit id state;
      state := newState
      if terminate? then return (leave id state, true, #[]) else
      let adjacencyList := (g.vertices[id].adjacencyList.map (λ edge => edge.target)).filter (!visited[.])
      let (newState, terminate?, newVisited) := g.depthFirstTraverseAux visit leave state adjacencyList visited n
      visited := newVisited
      state := leave id newState
      if terminate? then return (state, true, #[])

    return (state, false, visited)

def depthFirstTraversal4 (g : Graph α β) (sources : Array Nat) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (leave : Nat -> γ -> γ  := (λ _ x => x)) : γ :=
  (g.depthFirstTraverseAux visit leave startingState sources (mkArray g.vertices.size false) (g.vertices.size + 1)).1

open Tree

def createForest (sources : Array Nat) (g : Graph α β) : Array (Tree g) :=
  sources.map (λ id => nonExpandedNode id)

def createSpanningForest (g : Graph α β) := g.createForest g.getAllVertexIDs

def expandForest (forest : Array (Tree (g : Graph α β))) : Array (Tree g) :=
  let visited := mkArray g.vertices.size false
  let (forest, visited) := forest.foldr createForestAux (#[], visited)
  forest

def depthFirstTraversal3 (g : Graph α β) (sources : Array Nat) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (leave : Nat -> γ -> γ  := (λ _ x => x)) : γ :=
  let forest := g.createForest sources
  let res := forest.foldr (traverseForestAux visit leave) (startingState, mkArray g.vertices.size false)
  res.1

def depthFirstCompleteTraversal3 (g : Graph α β) (startingState : γ ) (visit : Nat -> γ -> γ × Bool) (leave : Nat -> γ -> γ  := (λ _ x => x)) : γ :=
  let forest := g.createSpanningForest
  let res := forest.foldr (traverseForestAux visit leave) (startingState, mkArray g.vertices.size false)
  res.1

end Graph