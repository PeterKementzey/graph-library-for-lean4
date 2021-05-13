import Graph.UndirectedGraph
import Std.Data.HashSet

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
    -- let currentEdge := remainingEdges.back
    -- let tree := _ -- find a tree in the forest that contains an edge that contains the source

    -- if found then check if it also contains the target, if it does then discard the edge since it would be a loop
    -- if it does not then find the tree that contains the target and merge the two sets and add the current edge

    -- sortedEdges.pop
    _

def kruskal (ug : UndirectedGraph α) : UndirectedGraph α := do
  let mut kruskalEdges : Array KruskalEdge := Array.empty
  for source in [0:ug.graph.vertices.size] do
    for edge in ug.graph.vertices[source].adjacencyList do
      kruskalEdges := kruskalEdges.push { source := source, target := edge.target, weight := edge.weight }

  let sortedEdges := kruskalEdges.insertionSort (λ l r => l.weight < r.weight)


  _


end UndirectedGraph end Graph