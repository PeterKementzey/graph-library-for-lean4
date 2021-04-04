import Graph.graphrepresentation

def exampleGraph : Graph Char :=
  let v0 : Vertex Char := { userData := 'a', adjacencyList := #[ {target := 1} ] }
  let v1 : Vertex Char := { userData := 'b', adjacencyList := #[ {target := 2} ] }
  let v2 : Vertex Char := { userData := 'c', adjacencyList := #[ {target := 3}, { target := 2}] }
  let v3 : Vertex Char := { userData := 'd', adjacencyList := #[ {target := 0} ] }
  ⟨#[v0, v1, v2, v3]⟩

-- def testing := 
--   let g1 : Graph Char := ⟨#[]⟩
--   let g2 := g1.addVertex 'a'
--   let g3 := g2.addVertex 'b'
--   let g4 := g3.addEdge 'a' 'b'
--   g4

def emptygraph: Graph Char := ⟨#[]⟩

def main : IO Unit :=
  IO.println exampleGraph
