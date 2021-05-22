import Graph.UndirectedGraph
import Std.Data.HashSet

import Graph.StdExtensions -- uses Std.Option.get! and Std.Data.HashSet.merge


namespace Graph namespace UndirectedGraph

variable {α : Type} [BEq α] [Inhabited α]

structure KruskalEdge where
  source : Nat
  target : Nat
  weight : Int := 1

instance : BEq KruskalEdge := ⟨ (fun l r => l.weight == r.weight && ((l.source == r.source && l.target == r.target) || (l.source == r.target && l.target == r.source))) ⟩
instance : Hashable KruskalEdge where hash e := mixHash (hash e.source) (mixHash (hash e.target) (hash e.weight))
instance : Inhabited KruskalEdge := ⟨ { source := arbitrary, target := arbitrary } ⟩

--                                               edges to add to spanning tree     connected vertex id's              resulting edges                            size of sortedEdges
private def kruskalAux (ug : UndirectedGraph α) (sortedEdges : Array KruskalEdge) (forest : Array (Std.HashSet Nat)) (spanningEdges : Std.HashSet KruskalEdge) : Nat -> Std.HashSet KruskalEdge
  | 0 => spanningEdges
  | n + 1 =>
    let currentEdge := sortedEdges[n-1]
    let sourceTreeId : Nat := (forest.findIdx? (fun tree => tree.contains currentEdge.source)).get!

    if forest[sourceTreeId].contains currentEdge.target then
      kruskalAux ug sortedEdges forest spanningEdges n
    else
      let sourceTree := forest[sourceTreeId]
      let forestWithoutSource := forest.eraseIdx sourceTreeId
      let targetTreeId : Nat := (forestWithoutSource.findIdx? (fun tree => tree.contains currentEdge.target)).get!
      let mergedTrees : Std.HashSet Nat := sourceTree.merge forestWithoutSource[targetTreeId]
      let newForest := (forestWithoutSource.eraseIdx targetTreeId).push mergedTrees
      let newSpanningEdges := spanningEdges.insert currentEdge
      kruskalAux ug sortedEdges newForest newSpanningEdges n

def kruskal (ug : UndirectedGraph α) : UndirectedGraph α := do
  let mut kruskalEdges : Array KruskalEdge := Array.empty
  let mut kruskalEdges : Std.HashSet KruskalEdge := Std.HashSet.empty
  for source in [0:ug.graph.vertices.size] do
    for edge in ug.graph.vertices[source].adjacencyList do
      kruskalEdges := kruskalEdges.insert { source := source, target := edge.target, weight := edge.weight }
  let sortedEdges := kruskalEdges.toArray.insertionSort (λ l r => l.weight > r.weight)

  let mut forest : Array (Std.HashSet Nat) := Array.empty
  for i in [0:ug.graph.vertices.size] do forest := forest.push (Std.HashSet.empty.insert i)

  let spanningEdges := kruskalAux ug sortedEdges forest Std.HashSet.empty sortedEdges.size
  spanningEdges.fold (λ spanningForest kruskalEdge => spanningForest.addEdgeById kruskalEdge.source kruskalEdge.target kruskalEdge.weight) ug.removeAllEdges

end UndirectedGraph end Graph