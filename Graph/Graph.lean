structure Edge where
  target : Nat
  weight : Int := 1

structure Vertex (α : Type) where
  userData : α
  adjacencyList : Array Edge := #[]

instance [Inhabited α] : Inhabited (Vertex α) := ⟨ { userData := arbitrary } ⟩ 

structure Graph (α : Type) where
  vertices : Array (Vertex α) := #[]

namespace Graph

variable {α : Type} [BEq α] [Inhabited α]

def empty {α : Type} : Graph α := ⟨#[]⟩ 

def addVertex (g : Graph α) (x : α): (Graph α) × Nat := 
  let res := { g with vertices := g.vertices.push { userData := x } }
  let id : Nat := res.vertices.size - 1
  (res, id)

def addEdgeById (g : Graph α) (source : Nat) (target : Nat) (weight : Int := 1) : Graph α := {
  g with vertices := g.vertices.modify source (fun vertex => { vertex with adjacencyList := vertex.adjacencyList.push {target := target, weight := weight} })
}

def getVertexPayload (g : Graph α) (id : Nat) : α := g.vertices[id].userData

def removeAllEdgesFromTo (g : Graph α) (source : Nat) (target : Nat) (weight : Option Int := none) : Graph α := { -- TODO test this
  g with vertices := g.vertices.modify source (λ vertex => { vertex with adjacencyList := vertex.adjacencyList.filter (λ edge => 
    match weight with
    | some w => (edge.weight != w) || edge.target != target
    | none => edge.target != target
  )})
}

def updateVertexPayload (g : Graph α) (id : Nat) (payload : α) : Graph α := {
  g with vertices := g.vertices.modify id (fun vertex => { vertex with userData := payload })
}

-- TODO removeVertex

instance : ToString (Edge) where toString e := "target: " ++ toString e.target ++ ", weight: " ++ toString e.weight
instance : ToString (Vertex α) where toString v := toString v.adjacencyList ++ "\n"
instance : ToString (Graph α) where toString g := toString g.vertices

end Graph




-- def findVertexId (g : Graph α) (userData : α) : Option Nat := g.vertices.findIdx? (fun v => v.userData == userData)



-- Test: can we define a circle with two vertices?
-- Answer: yes, if the current representation is what I think it is.
partial def circle : Graph Char :=
  let v0 : Vertex Char := { userData := 'a', adjacencyList := #[{target := 1}]}
  let v1 : Vertex Char := { userData := 'b', adjacencyList := #[{target := 0}]}
  ⟨#[v0, v1]⟩
  -- N.B. This is yet another way to construct values of structures. ⟨x, y, z⟩
  -- is the structure constructor (here Graph.mk) applied to x, y and z.
  -- Peter: this is cool but this character is not present on my US international keyboard :DD is there a backslash shortcut for it?
