import Graph.Graph
import Graph.Search
import Graph.Dijkstra

namespace Graph

structure UndirectedGraph (α : Type) where
  graph : Graph α

namespace UndirectedGraph

variable {α : Type} [BEq α] [Inhabited α]

def empty {α : Type} : UndirectedGraph α := ⟨ Graph.empty ⟩ 

def addVertex (ug : UndirectedGraph α) (x : α): (UndirectedGraph α) × Nat :=
  let (newGraph, id) := ug.graph.addVertex x
  ( ⟨ newGraph ⟩, id)

def addEdgeById (ug : UndirectedGraph α) (source : Nat) (target : Nat) (weight : Int := 1) : UndirectedGraph α :=
  let graphWithNewEdge := ug.graph.addEdgeById source target weight
  let graphWithOppositeEdge := if source != target then graphWithNewEdge.addEdgeById target source weight else graphWithNewEdge
  ⟨ graphWithOppositeEdge ⟩

def getVertexPayload (ug : UndirectedGraph α) := ug.graph.getVertexPayload

def removeAllEdgesFromTo (ug : UndirectedGraph α) (source : Nat) (target : Nat) (weight : Option Int := none) : UndirectedGraph α := 
  let graphWithEdgeRemoved := ug.graph.removeAllEdgesFromTo source target weight
  let graphWithOppositeEdgeRemoved := graphWithEdgeRemoved.removeAllEdgesFromTo target source weight
  ⟨ graphWithOppositeEdgeRemoved ⟩

def removeAllEdges (ug : UndirectedGraph α) : UndirectedGraph α := 
  let newGraph := ug.graph.removeAllEdges
  { graph := newGraph }

def updateVertexPayload (ug : UndirectedGraph α) (id : Nat) (payload : α) : UndirectedGraph α := ⟨
  ug.graph.updateVertexPayload id payload
⟩

def removeVertex (ug : UndirectedGraph α) (id : Nat) : (UndirectedGraph α) × (Nat -> Nat) := 
  let (newGraph, mapping) := ug.graph.removeVertex id
  (⟨ newGraph ⟩, mapping)


instance : ToString (UndirectedGraph α) where toString ug := toString ug.graph.vertices


-- Basic search

def breadthFirstSearch (ug : UndirectedGraph α) := ug.graph.breadthFirstSearch

def depthFirstSearch (ug : UndirectedGraph α) := ug.graph.depthFirstSearch

-- Dijkstra

def dijkstraUnsafe (ug : UndirectedGraph α) := ug.graph.dijkstraUnsafe

def dijkstraUnsafeWithDestination (ug : UndirectedGraph α) := ug.graph.dijkstraUnsafeWithDestination

end UndirectedGraph
end Graph