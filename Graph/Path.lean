namespace Graph

/-!
## Path

The `Path` inductive type is used to represent paths. It is defined as follows:

`inductive Path (β : Type) : Bool → Type where`
`  | vertex : Nat -> Path β false -> Path β true`
`  | edge : β -> Path β true -> Path β false`
`  | empty : ∀ {b}, Path β b`

You can match on a `Path` to parse it yourself or you can use the functions below.
-/

inductive Path (β : Type) : Bool → Type where
  | vertex : Nat -> Path β false -> Path β true
  | edge : β -> Path β true -> Path β false
  | empty : ∀ {b}, Path β b

namespace Path

private def toString [ToString β] : ∀ {b}, (Path β b) → String
  | true, vertex id p =>
    "vertex id: " ++ ToString.toString id ++ ", " ++ toString p
  | false, edge weight p =>
    "edge weight: " ++ ToString.toString weight ++ ", " ++ toString p
  | _, empty => "∎"

instance [ToString β] : ToString (Path β b) where toString p := toString p
instance : Inhabited (Path β b) where default := empty

/-- Returns the weight of the next edge in the path and the rest of the path without the edge (possibly empty). -/
def getNextEdgeWeight (p : Path β false) : (Option β) × Path β true := match p with
  | edge weight restOfThePath => (some weight, restOfThePath)
  | empty => (none, empty)

/-- Returns the ID of the next vertex in the path and the rest of the path without the vertex (possibly empty). -/
def getNextVertexID (p : Path β true) : (Option Nat) × Path β false := match p with
  | vertex id restOfThePath => (some id, restOfThePath)
  | empty => (none, empty)

/-- Returns an array of vertex IDs in the path in order. If `p` is a cycle this function will not terminate. -/
partial def toArray (p : Path β true) (arr : Array Nat := #[]) : Array Nat := match p with
  | vertex id restOfThePath => match restOfThePath with 
    | edge _ restOfThePath => restOfThePath.toArray (arr.push id)
    | empty => arr
  | empty => arr

end Path
end Graph