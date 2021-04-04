import Graph.graphrepresentation

def exampleGraph : Graph Char :=
  let v0 : Vertex Char := { userData := 'a', adjacencyList := #[ {target := 1} ] }
  let v1 : Vertex Char := { userData := 'b', adjacencyList := #[ {target := 2} ] }
  let v2 : Vertex Char := { userData := 'c', adjacencyList := #[ {target := 3}, { target := 2} ] }
  let v3 : Vertex Char := { userData := 'd', adjacencyList := #[ {target := 0} ] }
  ⟨#[v0, v1, v2, v3]⟩

def testing := 
  let gx : Graph Char := ⟨#[]⟩
  let (g0, id0) := gx.addVertex 'a'
  let (g1, id1) := g0.addVertex 'b'
  let g3 := g1.addEdgeById id1 id0
  let g4 := g3.addEdgeById id0 id0 5
  g4

def emptygraph: Graph Char := ⟨#[]⟩

def main : IO Unit :=
  IO.println testing
