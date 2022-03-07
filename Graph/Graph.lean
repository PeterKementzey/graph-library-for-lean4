/-!
## Graph

This module defines graphs and provides some basic construction functionality and properties. A `Graph (α : Type) (β : Type)` is a graph with vertex payloads of type `α` and edge weights of type `β`. If you don't want any of the algorithms to be imported to your code then import this module with `import Graph.Graph`.
-/

namespace Graph

structure Edge (β : Type) where
  target : Nat
  weight : β

structure Vertex (α : Type) (β : Type) where
  payload : α
  adjacencyList : Array (Edge β) := #[]

instance [Inhabited α] : Inhabited (Vertex α β) := ⟨ { payload := default } ⟩

end Graph

structure Graph (α : Type) (β : Type) where
  vertices : Array (Graph.Vertex α β) := #[]

namespace Graph

variable {α : Type} [Inhabited α] {β : Type}

/-- Empty graph, α is the vertex payload type, β is edge weight type. -/
def empty : Graph α β := ⟨#[]⟩

/-- Total edge count in the graph. -/
def edgeCount (g : Graph α β) : Nat := g.vertices.foldr (λ vertex count => vertex.adjacencyList.size + count) 0

def vertexCount (g : Graph α β) : Nat := g.vertices.size

/-- Returns the order of the graph. -/
def order (g : Graph α β) := g.vertexCount

/-- Returns true if the graph has no vertices. -/
def hasNoVertices (g : Graph α β) : Bool := g.vertices.isEmpty

/-- Returns true if the graph has no edges. -/
def hasNoEdges (g : Graph α β) : Bool := g.edgeCount == 0

/-- Add a vertex to the graph.
    Returns new graph and unique vertex ID. -/
def addVertex (g : Graph α β) (payload : α) : (Graph α β) × Nat :=
  let res := { g with vertices := g.vertices.push { payload := payload } }
  let id : Nat := res.vertexCount - 1
  (res, id)

def addEdgeByID (g : Graph α β) (source : Nat) (target : Nat) (weight : β) : Graph α β := {
  g with vertices := g.vertices.modify source (λ vertex => { vertex with adjacencyList := vertex.adjacencyList.push {target := target, weight := weight} })
}

/-- Creates a graph by mapping the array to vertices, indices in the array will be the respective node IDs, the elements will be the payload. -/
def makeGraphFromArray (a : Array α) : Graph α β := ⟨
  a.map (λ element => { payload := element } )
⟩

/-- Returns an array of vertex payloads in increasing order of IDs. -/
def toArray (g : Graph α β) : Array α := g.vertices.map (λ vertex => vertex.payload)

def outDegree (g : Graph α β) (id : Nat) : Nat := g.vertices[id].adjacencyList.size

def inDegree (g : Graph α β) (id : Nat) : Nat := g.vertices.foldr (λ vertex count => count + (vertex.adjacencyList.filter (λ edge => edge.target == id)).size) 0

def outDegrees (g : Graph α β) : Array Nat := g.vertices.map (λ vertex => vertex.adjacencyList.size)

def inDegrees (g : Graph α β) : Array Nat := Id.run do
  let mut res : Array Nat := mkArray g.vertexCount 0
  for vertex in g.vertices do
    for edge in vertex.adjacencyList do
      res := res.modify edge.target (.+1)
  res

/-- Returns an array of vertex IDs in increasing order. -/
def getAllVertexIDs (g : Graph α β) : Array Nat := Id.run do
  let mut arr := mkArray g.vertexCount 0
  for i in [0:g.vertexCount] do arr := arr.set! i i
  arr

/-- Removes all edges from source to target with specific weight.
    If weight is not specified all edges from source to target are removed. -/
def removeAllEdgesFromTo [BEq β] (g : Graph α β) (source : Nat) (target : Nat) (weight : Option β := none) : Graph α β := {
  g with vertices := g.vertices.modify source (λ vertex => { vertex with adjacencyList := vertex.adjacencyList.filter (λ edge =>
    match weight with
    | some w => (edge.weight != w) || edge.target != target
    | none => edge.target != target
  )})
}

/-- Removes all edges from the entire graph -/
def removeAllEdges (g : Graph α β) : Graph α β := {
  g with vertices := g.vertices.map (λ vertex => { vertex with adjacencyList := Array.empty })
}

def getVertexPayload (g : Graph α β) (id : Nat) : α := g.vertices[id].payload

def updateVertexPayload (g : Graph α β) (id : Nat) (payload : α) : Graph α β := {
  g with vertices := g.vertices.modify id (λ vertex => { vertex with payload := payload })
}

/-- Replaces all vertex payloads to the new payloads. The payloads array should have the same length as the number of vertices. -/
def updateAllVertexPayloads (g : Graph α β) (payloads : Array γ) : Option (Graph γ β) :=
  if payloads.size != g.vertexCount then none else some ⟨
    (g.vertices.zip payloads).map (λ (vertex, newPayload) => { vertex with payload := newPayload })
  ⟩

/-- Returns an array of vertex IDs whose payload equals the payload parameter. -/
def findVertexIDs [BEq α] (g : Graph α β) (payload : α) : Array Nat := g.getAllVertexIDs.filter (λ id => g.vertices[id].payload == payload)

/-- Warning! This function is deprecated, vertex IDs will change if used.
    Returns graph without vertex and a mapping from old vertex IDs to new vertex IDs. -/
def removeVertex (g : Graph α β) (id : Nat) : (Graph α β) × (Nat -> Nat) :=
  let mapping : Nat -> Nat := mappingBase id
  let verticesWithEdgesRemoved := g.vertices.map (λ vertex => {
    vertex with adjacencyList := vertex.adjacencyList.filter (λ edge => edge.target != id)
  })
  let verticesWithMapping := verticesWithEdgesRemoved.map (λ vertex => {
    vertex with adjacencyList := vertex.adjacencyList.map (λ edge => {
      edge with target := mapping edge.target
    })
  })
  let verticesWithVertexRemoved := verticesWithMapping.eraseIdx id
  (⟨ verticesWithVertexRemoved ⟩, mapping)
  where
    mappingBase (id : Nat) (x : Nat) : Nat := if x > id then x - 1 else x

/-- Map vertex payloads. -/
def mapVertices [Inhabited γ] (g : Graph α β) (f : α -> γ) : Graph γ β := ⟨
  g.vertices.map (λ vertex => { vertex with payload := f vertex.payload })
⟩

/-- Map edge weights. -/
def mapEdges (g : Graph α β) (f : β -> γ) : Graph α γ := ⟨
  g.vertices.map (λ vertex => { vertex with adjacencyList := vertex.adjacencyList.map (λ edge =>
    { edge with weight := f edge.weight }
  )})
⟩

namespace Vertex

private def toString [ToString α] [ToString β] (v : Vertex α β) : String := "\nVertex payload: " ++ ToString.toString v.payload ++ ", edges:\n" ++ v.adjacencyList.foldr foldEdges "" ++ "\n"
  where foldEdges (e : Edge β) (s : String) : String :=
    s ++ "   target: " ++ (ToString.toString e.target) ++ ", weight: " ++ (ToString.toString e.weight) ++ "\n"

instance [ToString α] [ToString β] : ToString (Vertex α β) where toString := toString

end Vertex

instance [ToString α] [ToString β] : ToString (Graph α β) where toString :=
  (λ g => toString (g.getAllVertexIDs.zip g.vertices))

end Graph
