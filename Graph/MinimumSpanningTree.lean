import Graph.UndirectedGraph
import Std.Data.HashSet

namespace Std namespace Option

def get! {α : Type _} [Inhabited α] : Option α -> α
  | some x => x
  | none => panic! "This cannot be none"

end Option namespace HashSet

def merge {α : Type u} [BEq α] [Hashable α] (l : HashSet α) (r : HashSet α) : HashSet α := r.fold insert l

end HashSet end Std


namespace Graph namespace UndirectedGraph

variable {α : Type} [BEq α] [Inhabited α]

structure KruskalEdge where
  source : Nat
  target : Nat
  weight : Int := 1

instance : BEq KruskalEdge := ⟨ (fun l r => l.weight == r.weight && l.source == r.source && l.target == r.target) ⟩
instance : Hashable KruskalEdge where hash e := mixHash (hash e.source) (mixHash (hash e.target) (hash e.weight))
instance : Inhabited KruskalEdge := ⟨ { source := arbitrary, target := arbitrary } ⟩

--                                               edges to add to spanning tree        connected vertex id's              resulting edges                            size of remainingEdges
private def kruskalAux (ug : UndirectedGraph α) (remainingEdges : Array KruskalEdge) (forest : Array (Std.HashSet Nat)) (spanningEdges : Std.HashSet KruskalEdge) : Nat -> Std.HashSet KruskalEdge
  | 0 => spanningEdges
  | n + 1 =>
    let currentEdge := remainingEdges.back -- Question: is this efficient or is there some kind of head like thing
    let sourceTreeId : Nat := (forest.findIdx? (fun tree => tree.contains currentEdge.source)).get!

    if forest[sourceTreeId].contains currentEdge.target then
      kruskalAux ug remainingEdges.pop forest spanningEdges n
    else
      let sourceTree := forest[sourceTreeId]
      let forestWithoutSource := forest.eraseIdx sourceTreeId
      let targetTreeId : Nat := (forestWithoutSource.findIdx? (fun tree => tree.contains currentEdge.target)).get!
      let mergedTrees : Std.HashSet Nat := sourceTree.merge forestWithoutSource[targetTreeId]
      let newForest := (forestWithoutSource.eraseIdx targetTreeId).push mergedTrees
      let newSpanningEdges := spanningEdges.insert currentEdge
      kruskalAux ug remainingEdges.pop newForest newSpanningEdges n

def kruskal (ug : UndirectedGraph α) : UndirectedGraph α := do
  let mut kruskalEdges : Array KruskalEdge := Array.empty
  for source in [0:ug.graph.vertices.size] do
    for edge in ug.graph.vertices[source].adjacencyList do
      kruskalEdges := kruskalEdges.push { source := source, target := edge.target, weight := edge.weight }

  let sortedEdges := kruskalEdges.insertionSort (λ l r => l.weight < r.weight) -- TODO this might not be ther right way around


  _


end UndirectedGraph end Graph