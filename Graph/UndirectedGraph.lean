import Graph.Graph
import Graph.Search
import Graph.Dijkstra

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


-- Basic search

def breadthFirstSearch (ug : UndirectedGraph α) (source : Nat) (target : Nat) : Bool :=
  ug.graph.breadthFirstSearch source target

def depthFirstSearch (ug : UndirectedGraph α) (source : Nat) (target : Nat) : Bool :=
  ug.graph.depthFirstSearch source target

-- Dijkstra

def dijkstraUnsafe (ug : UndirectedGraph α) (source : Nat) : ShortestPathTree :=
  ug.graph.dijkstraUnsafe source

def dijkstraUnsafeWithDestination (ug : UndirectedGraph α) (source : Nat) (target : Nat) : Option (ShortestPathTree.Path true) := 
  ug.graph.dijkstraUnsafeWithDestination source target

end UndirectedGraph
end Graph