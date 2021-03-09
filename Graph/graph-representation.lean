
structure Edge (α : Type) where
  source : α 
  target : α 
  -- maybe just add weight here with default value 1?

structure WeightedEdge (α : Type) extends Edge α where
  weight : Nat


structure Vertex (α : Type) where
  node : α 
  adjacencyList : Array (Edge α) := Array.empty


structure Graph (α : Type) (lt : α → α → Bool) where
  vertices : Array (Vertex α) := Array.empty


namespace Graph

variables {α : Type} {β : α → α → Bool}

def addVertex (g : Graph α β) (x : α): Graph α β :=
  { g with vertices := match g.vertices.findIdx? (g.lt x) with
    | some index => g.vertices.insertAt index (Vertex x)
    | none => g.vertices.insertAt 0 (Vertex x)
   }

-- def mkFromVertexList (vertices: Array α) : γ := 0

-- def addEdge

end Graph

