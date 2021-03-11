structure Edge where
  target : Nat
  weight : Int := 1

structure Vertex (α : Type) where
  userData : α
  adjacencyList : Array Edge := #[]

structure Graph (α : Type) where
  vertices : Array (Vertex α) := #[]

namespace Graph

variables {α : Type}

def addVertex (g : Graph α) (x : α): Graph α := {
  g with vertices := g.vertices.push {userData := x}
}

-- def addEdge

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
