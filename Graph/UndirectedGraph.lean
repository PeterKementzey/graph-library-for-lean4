import Graph.Graph

namespace Graph

structure UndirectedGraph (α : Type) (β : Type) where
  graph : Graph α β

namespace UndirectedGraph

variable {α : Type} [BEq α] [Inhabited α] variable {β : Type}

/-- Empty undirected graph, α is vertex payload type, β is edge weight type. -/
def empty {α : Type} : UndirectedGraph α β := ⟨ Graph.empty ⟩

/-- Add a vertex to the graph.
    Returns new graph and unique vertex ID. -/
def addVertex (ug : UndirectedGraph α β) (x : α): (UndirectedGraph α β) × Nat :=
  let (newGraph, id) := ug.graph.addVertex x
  ( ⟨ newGraph ⟩, id)

/-- -/
def addEdgeById [DefaultEdgeWeight β] (ug : UndirectedGraph α β) (source : Nat) (target : Nat) (weight : β := DefaultEdgeWeight.default) : UndirectedGraph α β :=
  let graphWithNewEdge := ug.graph.addEdgeById source target weight
  let graphWithOppositeEdge := if source != target then graphWithNewEdge.addEdgeById target source weight else graphWithNewEdge
  ⟨ graphWithOppositeEdge ⟩

/-- -/
def getVertexPayload (ug : UndirectedGraph α β) := ug.graph.getVertexPayload

/-- Removes all edges between x and y with specific weight, order of vertex IDs does not matter.
    If weight is not specified all edges between x and y are removed. -/
def removeAllEdgesFromTo [BEq β] (ug : UndirectedGraph α β) (x : Nat) (y : Nat) (weight : Option β := none) : UndirectedGraph α β :=
  let graphWithEdgeRemoved := ug.graph.removeAllEdgesFromTo x y weight
  let graphWithOppositeEdgeRemoved := graphWithEdgeRemoved.removeAllEdgesFromTo x y weight
  ⟨ graphWithOppositeEdgeRemoved ⟩

/-- Removes all edges from the entire graph -/
def removeAllEdges (ug : UndirectedGraph α β) : UndirectedGraph α β :=
  let newGraph := ug.graph.removeAllEdges
  { graph := newGraph }

/-- -/
def updateVertexPayload (ug : UndirectedGraph α β) (id : Nat) (payload : α) : UndirectedGraph α β := ⟨
  ug.graph.updateVertexPayload id payload
⟩

/-- Warning! This function is deprecated, vertex IDs will change if used.
    Returns graph without vertex and a mapping from old vertex IDs to new vertex IDs. -/
def removeVertex (ug : UndirectedGraph α β) (id : Nat) : (UndirectedGraph α β) × (Nat -> Nat) :=
  let (newGraph, mapping) := ug.graph.removeVertex id
  (⟨ newGraph ⟩, mapping)


instance [ToString α] [ToString β] : ToString (UndirectedGraph α β) where toString ug := toString ug.graph
instance [ToString α] [ToString β] : ToString (UndirectedGraph α β) where toString ug := toString ug.graph

end UndirectedGraph
end Graph