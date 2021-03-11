/-
I'm overall a bit confused about the representation. Is the α a unique
identifier for each vertex?

If so, I would make this a Nat instead of some arbitrary type. Then you can
store the adjacency information very efficiently as an array A where A[n] is
the adjacency list of node n. However, you probably want to allow vertices to
store some information in addition to their unique identifier.

If the α is not supposed to be a unique identifier, then your current definition
of Edge doesn't make sense to me since the source and target are not uniquely
identified.

Another way to store the adjacency information would be as a map (e.g. a hash
map) which maps nodes to the their lists of neighbours. Then your nodes can be
arbitrary (hashable) things. However, I would expect this to be somewhat slower
in practice than the array solution, even if the complexity is the same.

Wikipedia [1] has some more details on these representations. Which one you
choose imo doesn't matter that much for this project so long as it is at least
reasonably efficient.

[1] https://en.wikipedia.org/wiki/Adjacency_list
-/

structure Edge (α : Type) where
  source : α
  target : α
  -- maybe just add weight here with default value 1?

structure WeightedEdge (α : Type) extends Edge α where
  weight : Nat


structure Vertex (α : Type) where
  node : α
  adjacencyList : Array (Edge α) := #[]
  -- Why is the adjacency list a list of edges? Shouldn't it be a list of
  -- vertices, since the source vertex of each edge is the current vertex?


structure Graph (α : Type) (lt : α → α → Bool) where
  vertices : Array (Vertex α) := #[]

-- Test: can we define a circle with two vertices?
-- Answer: yes, if the current representation is what I think it is.
partial def circle : Graph Nat (. < .) :=
  let v0 : Vertex Nat := { node := 0, adjacencyList := #[{source := 0, target := 1}]}
  let v1 : Vertex Nat := { node := 1, adjacencyList := #[{source := 1, target := 0}]}
  ⟨#[v0, v1]⟩
  -- N.B. This is yet another way to construct values of structures. ⟨x, y, z⟩
  -- is the structure constructor (here Graph.mk) applied to x, y and z.


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
