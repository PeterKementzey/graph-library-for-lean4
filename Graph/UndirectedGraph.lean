import Graph.Graph
import Graph.Search
import Graph.Dijkstra

namespace Graph

structure UndirectedGraph (α : Type) (β : Type) where
  graph : Graph α β

namespace UndirectedGraph

variable {α : Type} [BEq α] [Inhabited α] variable {β : Type}

def empty {α : Type} : UndirectedGraph α β := ⟨ Graph.empty ⟩

def addVertex (ug : UndirectedGraph α β) (x : α): (UndirectedGraph α β) × Nat :=
  let (newGraph, id) := ug.graph.addVertex x
  ( ⟨ newGraph ⟩, id)

def addEdgeById [DefaultEdgeWeight β] (ug : UndirectedGraph α β) (source : Nat) (target : Nat) (weight : β := DefaultEdgeWeight.default) : UndirectedGraph α β :=
  let graphWithNewEdge := ug.graph.addEdgeById source target weight
  let graphWithOppositeEdge := if source != target then graphWithNewEdge.addEdgeById target source weight else graphWithNewEdge
  ⟨ graphWithOppositeEdge ⟩

def getVertexPayload (ug : UndirectedGraph α β) := ug.graph.getVertexPayload

def removeAllEdgesFromTo [BEq β] (ug : UndirectedGraph α β) (source : Nat) (target : Nat) (weight : Option β := none) : UndirectedGraph α β :=
  let graphWithEdgeRemoved := ug.graph.removeAllEdgesFromTo source target weight
  let graphWithOppositeEdgeRemoved := graphWithEdgeRemoved.removeAllEdgesFromTo target source weight
  ⟨ graphWithOppositeEdgeRemoved ⟩

def removeAllEdges (ug : UndirectedGraph α β) : UndirectedGraph α β :=
  let newGraph := ug.graph.removeAllEdges
  { graph := newGraph }

def updateVertexPayload (ug : UndirectedGraph α β) (id : Nat) (payload : α) : UndirectedGraph α β := ⟨
  ug.graph.updateVertexPayload id payload
⟩

def removeVertex (ug : UndirectedGraph α β) (id : Nat) : (UndirectedGraph α β) × (Nat -> Nat) :=
  let (newGraph, mapping) := ug.graph.removeVertex id
  (⟨ newGraph ⟩, mapping)


instance : ToString (UndirectedGraph α Nat) where toString ug := toString ug.graph.vertices
instance : ToString (UndirectedGraph α β) where toString ug := toString ug.graph.vertices


-- Basic search

def breadthFirstSearch (ug : UndirectedGraph α β) := ug.graph.breadthFirstSearch

def depthFirstSearch (ug : UndirectedGraph α β) := ug.graph.depthFirstSearch

-- Dijkstra

def dijkstraUnsafe (ug : UndirectedGraph α Nat) := ug.graph.dijkstraUnsafe

def dijkstraUnsafeWithTarget (ug : UndirectedGraph α Nat) := ug.graph.dijkstraUnsafeWithTarget

end UndirectedGraph
end Graph