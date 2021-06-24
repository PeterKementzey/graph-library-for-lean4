import Graph.Graph

/-!
## Undirected graph

This module defines graphs and provides some basic construction functionality and properties. A `Graph (α : Type) (β : Type)` is a graph with vertex payloads of type `α` and edge weights of type `β`. If you don't want any of the algorithms to be imported to your code then import this module with `import Graph.UndirectedGraph`.
-/

namespace Graph

structure UndirectedGraph (α : Type) (β : Type) where
  graph : Graph α β

namespace UndirectedGraph

variable {α : Type} [Inhabited α] {β : Type}

/-- Empty undirected graph, α is vertex payload type, β is edge weight type. -/
def empty {α : Type} : UndirectedGraph α β := ⟨ Graph.empty ⟩

/-- Total edge count in the graph. Note that while the undirected graph representation contains all edges in both directions, this function returns only half of that, so the expected edge count. -/
def edgeCount (ug : UndirectedGraph α β) : Nat := ug.graph.edgeCount / 2

def vertexCount (ug : UndirectedGraph α β) : Nat := ug.graph.vertexCount

/-- Returns the order of the graph. -/
def order (ug : UndirectedGraph α β) := ug.vertexCount

/-- Returns true if the graph has no vertices. -/
def hasNoVertices (ug : UndirectedGraph α β) : Bool := ug.graph.hasNoVertices

/-- Returns true if the graph has no edges. -/
def hasNoEdges (ug : UndirectedGraph α β) : Bool := ug.graph.hasNoEdges

/-- Add a vertex to the graph.
    Returns new graph and unique vertex ID. -/
def addVertex (ug : UndirectedGraph α β) (x : α): (UndirectedGraph α β) × Nat :=
  let (newGraph, id) := ug.graph.addVertex x
  ( ⟨ newGraph ⟩, id)

def addEdgeByID (ug : UndirectedGraph α β) (source : Nat) (target : Nat) (weight : β) : UndirectedGraph α β :=
  let graphWithNewEdge := ug.graph.addEdgeByID source target weight
  let graphWithOppositeEdge := if source != target then graphWithNewEdge.addEdgeByID target source weight else graphWithNewEdge
  ⟨ graphWithOppositeEdge ⟩

/-- Creates a graph by mapping the array to vertices, indices in the array will be the respective node IDs, the elements will be the payload. -/
def makeUndirectedGraphFromArray (a : Array α) : UndirectedGraph α β := ⟨
    makeGraphFromArray a
⟩

/-- Returns an array of vertex payloads in increasing order of IDs. -/
def toArray (ug : UndirectedGraph α β) : Array α := ug.graph.toArray

def degree (ug : UndirectedGraph α β) (id : Nat) : Nat := ug.graph.vertices[id].adjacencyList.size

def getVertexPayload (ug : UndirectedGraph α β) := ug.graph.getVertexPayload

def getAllVertexIDs (ug : UndirectedGraph α β) : Array Nat := ug.graph.getAllVertexIDs

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

def updateVertexPayload (ug : UndirectedGraph α β) (id : Nat) (payload : α) : UndirectedGraph α β := ⟨
  ug.graph.updateVertexPayload id payload
⟩

/-- Warning! This function is deprecated, vertex IDs will change if used.
    Returns graph without vertex and a mapping from old vertex IDs to new vertex IDs. -/
def removeVertex (ug : UndirectedGraph α β) (id : Nat) : (UndirectedGraph α β) × (Nat -> Nat) :=
  let (newGraph, mapping) := ug.graph.removeVertex id
  (⟨ newGraph ⟩, mapping)

/-- Map vertex payloads. -/
def mapVertices [Inhabited γ] (ug : UndirectedGraph α β) (f : α -> γ) : UndirectedGraph γ β := ⟨
  ug.graph.mapVertices f
⟩

/-- Map edge weights. -/
def mapEdges [Inhabited γ] (ug : UndirectedGraph α β) (f : β -> γ) : UndirectedGraph α γ := ⟨
  ug.graph.mapEdges f
⟩

instance [ToString α] [ToString β] : ToString (UndirectedGraph α β) where toString ug := toString ug.graph

end UndirectedGraph
end Graph