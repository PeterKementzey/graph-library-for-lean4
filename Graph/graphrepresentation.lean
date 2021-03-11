structure Edge where
  target : Nat
  weight : Int := 1

structure Vertex (α : Type) where
  userData : α
  adjacencyList : Array Edge := #[]

structure Graph (α : Type) where
  vertices : Array (Vertex α) := #[]

namespace Graph

variables {α : Type} [BEq α] [Inhabited (Vertex α)] -- Question: is this a problem? Do I need to do something about it?

def addVertex (g : Graph α) (x : α): Graph α := {
  g with vertices := g.vertices.push {userData := x}
}

def findVertexId (g : Graph α) (userData : α) : Nat := match g.vertices.findIdx? (fun v => v.userData == userData) with -- Question: Is there a findIdx! that I could use? It did not work for some reason
  | some x => x
  | none => panic! "Vertex not found" -- Question: I saw this somewhere but I am not sure what it does

def addEdgeById (g : Graph α) (source : Nat) (target : Nat) (weight : Int := 1) : Graph α := {
  g with vertices := g.vertices.modify source (fun vertex => { vertex with adjacencyList := vertex.adjacencyList.push {target := target, weight := weight} })
}
-- TODO: It would probably be easier to have a separate userData and adjacencyLists arrays in Graph replacing the vertices array because this way it is complicated to access and modify stuff

def addEdge (g : Graph α) (source : α) (target : α) := g.addEdgeById (g.findVertexId source) (g.findVertexId target)
-- #check Graph.addEdge -- I think this is not saying what it should say

instance : ToString (Graph α) where toString g := "graph"

end Graph







-- Test: can we define a circle with two vertices?
-- Answer: yes, if the current representation is what I think it is.
partial def circle : Graph Char :=
  let v0 : Vertex Char := { userData := 'a', adjacencyList := #[{target := 1}]}
  let v1 : Vertex Char := { userData := 'b', adjacencyList := #[{target := 0}]}
  ⟨#[v0, v1]⟩
  -- N.B. This is yet another way to construct values of structures. ⟨x, y, z⟩
  -- is the structure constructor (here Graph.mk) applied to x, y and z.
  -- Peter: this is cool but this character is not present on my US international keyboard :DD is there a backslash shortcut for it?
