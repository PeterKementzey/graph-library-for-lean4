import Graph.Graph

namespace Graph

variable {α : Type} [Inhabited α] {β : Type}

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

end Graph