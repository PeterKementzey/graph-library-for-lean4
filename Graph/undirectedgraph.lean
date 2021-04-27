import Graph.graphrepresentation

namespace Graph

structure UndirectedGraph (α : Type) where
  graph : Graph α

namespace UndirectedGraph

variable {α : Type} [BEq α] [Inhabited α]

def empty {α : Type} : UndirectedGraph α := ⟨Graph.empty⟩ 

def addVertex (ug : UndirectedGraph α) (x : α): (UndirectedGraph α) × Nat :=
  let (newGraph, id) := ug.graph.addVertex x
  ( {graph := newGraph}, id)

def addEdgeById (ug : UndirectedGraph α) (source : Nat) (target : Nat) (weight : Int := 1) : UndirectedGraph α :=
  let graphWithNewEdge := ug.graph.addEdgeById source target weight
  let graphWithOppositeEdge := graphWithNewEdge.addEdgeById target source weight
  { graph := graphWithOppositeEdge }

instance : ToString (UndirectedGraph α) where toString ug := toString ug.graph.vertices

end UndirectedGraph
end Graph