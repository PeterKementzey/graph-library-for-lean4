import Graph.UndirectedGraph
import Lean.Data.HashSet
import Std

/-!
## Kruskal's algorithm

Finds a minimum spanning forest in an undirected graph.
-/

namespace Graph namespace UndirectedGraph

variable {α : Type} [Inhabited α] {β : Type} [BEq β] [Hashable β] [Inhabited β]

private structure KruskalEdge (β : Type) where
  source : Nat
  target : Nat
  weight : β

private instance : BEq (KruskalEdge β) := ⟨ (λ l r => l.weight == r.weight && ((l.source == r.source && l.target == r.target) || (l.source == r.target && l.target == r.source))) ⟩
private instance : Hashable (KruskalEdge β) where hash e := mixHash (hash e.source) (mixHash (hash e.target) (hash e.weight))
private instance : Inhabited (KruskalEdge β) := ⟨ { source := default, target := default, weight := default } ⟩

private def mergeSets {α : Type u} [BEq α] [Hashable α] (l : Lean.HashSet α) (r : Lean.HashSet α) : Lean.HashSet α := r.fold Lean.HashSet.insert l

--                                                 edges to add to spanning tree         connected vertex id's              resulting edges                                size of sortedEdges
private def kruskalAux (ug : UndirectedGraph α β) (sortedEdges : Array (KruskalEdge β)) (forest : Array (Lean.HashSet Nat)) (spanningEdges : Lean.HashSet (KruskalEdge β)) : Nat -> Lean.HashSet (KruskalEdge β)
  | 0 => spanningEdges
  | n + 1 =>
    let currentEdge := sortedEdges[n-1]!
    let sourceTreeId : Nat := (forest.findIdx? (λ tree => tree.contains currentEdge.source)).get!

    if forest[sourceTreeId]!.contains currentEdge.target then
      kruskalAux ug sortedEdges forest spanningEdges n
    else
      let sourceTree := forest[sourceTreeId]!
      let forestWithoutSource := forest.eraseIdx sourceTreeId
      let targetTreeId : Nat := (forestWithoutSource.findIdx? (λ tree => tree.contains currentEdge.target)).get!
      let mergedTrees : Lean.HashSet Nat := mergeSets sourceTree forestWithoutSource[targetTreeId]!
      let newForest := (forestWithoutSource.eraseIdx targetTreeId).push mergedTrees
      let newSpanningEdges := spanningEdges.insert currentEdge
      kruskalAux ug sortedEdges newForest newSpanningEdges n

/-- Kruskal's algorithm to find a minimum spanning forest in an undirected graph. If the graph is connected, it finds a minimum spanning tree. -/
def kruskal (ug : UndirectedGraph α β) (lt : β -> β -> Bool) : UndirectedGraph α β := Id.run do
  let mut kruskalEdges : Lean.HashSet (KruskalEdge β) := Lean.HashSet.empty
  for source in ug.getAllVertexIDs do
    for edge in ug.graph.vertices[source]!.adjacencyList do
      kruskalEdges := kruskalEdges.insert { source := source, target := edge.target, weight := edge.weight }
  let sortedEdges := kruskalEdges.toArray.qsort (λ l r => lt r.weight l.weight) -- (λ l r => l.weight < r.weight)

  let mut forest : Array (Lean.HashSet Nat) := Array.empty
  for i in ug.getAllVertexIDs do forest := forest.push (Lean.HashSet.empty.insert i)

  let spanningEdges := kruskalAux ug sortedEdges forest Lean.HashSet.empty sortedEdges.size
  spanningEdges.fold (λ spanningForest kruskalEdge => spanningForest.addEdgeByID kruskalEdge.source kruskalEdge.target kruskalEdge.weight) ug.removeAllEdges

end UndirectedGraph end Graph
