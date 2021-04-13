import Graph.graphrepresentation
import Graph.dijkstra
-- Question: Can I include all in one line?

def exampleGraph : Graph Char :=
  let v0 : Vertex Char := { userData := 'a', adjacencyList := #[ {target := 1} ] }
  let v1 : Vertex Char := { userData := 'b', adjacencyList := #[ {target := 2} ] }
  let v2 : Vertex Char := { userData := 'c', adjacencyList := #[ {target := 3}, { target := 2} ] }
  let v3 : Vertex Char := { userData := 'd', adjacencyList := #[ {target := 0} ] }
  ⟨#[v0, v1, v2, v3]⟩

def exampleGraph2 : Graph Char := 
  let v0 : Vertex Char := {
    userData := 'S',
    adjacencyList := #[
      {target := 1, weight := 4},
      {target := 2, weight := 9},
      {target := 3, weight := 6}
    ]
  }
  let v1 : Vertex Char := {
    userData := 'A',
    adjacencyList := #[
      {target := 0, weight := 4},
      {target := 2, weight := 4},
      {target := 4, weight := 9}
    ]
  }
  let v2 : Vertex Char := {
    userData := 'B',
    adjacencyList := #[
      {target := 0, weight := 9},
      {target := 1, weight := 4},
      {target := 3, weight := 3},
      {target := 4, weight := 1}
    ]
  }
  let v3 : Vertex Char := {
    userData := 'C',
    adjacencyList := #[
      {target := 0, weight := 6},
      {target := 2, weight := 3},
      {target := 4, weight := 5}
    ]
  }
  let v4 : Vertex Char := {
    userData := 'T',
    adjacencyList := #[
      {target := 1, weight := 9},
      {target := 2, weight := 1},
      {target := 3, weight := 5}
    ]
  }
  ⟨#[v0, v1, v2, v3, v4]⟩

def testing := 
  let gx : Graph Char := ⟨#[]⟩
  let (g0, id0) := gx.addVertex 'a'
  let (g1, id1) := g0.addVertex 'b'
  let g3 := g1.addEdgeById id1 id0
  let g4 := g3.addEdgeById id0 id0 5
  g4

def exampleGraph3 : Graph Nat := do
  let mut gx : Graph Nat := ⟨#[]⟩
  gx := (gx.addVertex 0).1
  gx := (gx.addVertex 1).1
  gx := (gx.addVertex 2).1
  gx := (gx.addVertex 3).1
  gx := (gx.addVertex 4).1
  gx := gx.addEdgeById 0 1 2
  gx := gx.addEdgeById 0 1 2
  gx := gx.addEdgeById 0 2 5
  gx := gx.addEdgeById 1 2 1
  gx := gx.addEdgeById 1 1 1
  gx := gx.addEdgeById 3 4 5
  gx := gx.addEdgeById 4 3 9
  gx

def emptygraph: Graph Char := ⟨#[]⟩

def main : IO Unit :=
  IO.println (exampleGraph3.dijkstraUnsafe 0)
