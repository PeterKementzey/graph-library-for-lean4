import Graph.Graph
import Graph.Container

namespace Graph

open Internal

variable {α : Type} [BEq α] [Inhabited α]

private def searchAux (g : Graph α) (target : Nat) (visited : Array Bool) (container : Container Nat β) : Nat -> Bool
  | 0 => false
  | n+1 => match container.remove? with
    | none => false
    | some (current, containerWithNodeRemoved) => do
      let mut visitedMut := visited
      let unvisitedNeighborIds := (g.vertices[current].adjacencyList.map (λ e => e.target)).filter (λ e => !visitedMut[e])
      if unvisitedNeighborIds.contains target then true
      else
        for neighbor in unvisitedNeighborIds do
          visitedMut := visitedMut.set! neighbor true
        let containerWithNewNodes := containerWithNodeRemoved.addAll unvisitedNeighborIds
        searchAux g target visitedMut containerWithNewNodes n


def breadthFirstSearchOld (g : Graph α) (source : Nat) (target : Nat) : Bool :=
  if source == target then true
  else
    let visited : Array Bool := mkArray g.vertices.size false
    searchAux g target (visited.set! source true) (Container.emptyQueue.add source) g.vertices.size

def depthFirstSearchOld (g : Graph α) (source : Nat) (target : Nat) : Bool :=
  if source == target then true
  else
    let visited : Array Bool := mkArray g.vertices.size false
    searchAux g target (visited.set! source true) (Container.emptyStack.add source) g.vertices.size

end Graph
