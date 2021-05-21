import Graph.All

def exampleGraph1 : Graph Char :=
  let v0 : Vertex Char := { userData := 'a', adjacencyList := #[ {target := 1} ] }
  let v1 : Vertex Char := { userData := 'b', adjacencyList := #[ {target := 2} ] }
  let v2 : Vertex Char := { userData := 'c', adjacencyList := #[ {target := 3}, { target := 2} ] }
  let v3 : Vertex Char := { userData := 'd', adjacencyList := #[ {target := 0} ] }
  ⟨#[v0, v1, v2, v3]⟩

def exampleGraph2 : Graph Char := 
  let v0 : Vertex Char := {
    userData := '0',
    adjacencyList := #[
      {target := 1, weight := 4},
      {target := 2, weight := 9},
      {target := 3, weight := 6}
    ]
  }
  let v1 : Vertex Char := {
    userData := '1',
    adjacencyList := #[
      {target := 0, weight := 4},
      {target := 2, weight := 4},
      {target := 4, weight := 9}
    ]
  }
  let v2 : Vertex Char := {
    userData := '2',
    adjacencyList := #[
      {target := 0, weight := 9},
      {target := 1, weight := 4},
      {target := 3, weight := 3},
      {target := 4, weight := 1}
    ]
  }
  let v3 : Vertex Char := {
    userData := '3',
    adjacencyList := #[
      {target := 0, weight := 6},
      {target := 2, weight := 3},
      {target := 4, weight := 5}
    ]
  }
  let v4 : Vertex Char := {
    userData := '4',
    adjacencyList := #[
      {target := 1, weight := 9},
      {target := 2, weight := 1},
      {target := 3, weight := 5}
    ]
  }
  ⟨#[v0, v1, v2, v3, v4]⟩

def exampleGraph3 : Graph Nat := do
  let mut gx : Graph Nat := ⟨#[]⟩
  gx := (gx.addVertex 0).1
  gx := (gx.addVertex 1).1
  gx := (gx.addVertex 2).1
  gx := (gx.addVertex 3).1
  gx := (gx.addVertex 4).1
  gx := gx.addEdgeById 0 1 2
  gx := gx.addEdgeById 0 2 5
  gx := gx.addEdgeById 1 2 1
  gx := gx.addEdgeById 1 1 1
  gx := gx.addEdgeById 3 4 5
  gx := gx.addEdgeById 4 3 9
  gx

def exampleGraph4 : Graph Nat := do
  let mut gx : Graph Nat := Graph.empty
  gx := (gx.addVertex 0).1
  gx := (gx.addVertex 1).1
  gx := (gx.addVertex 2).1
  gx := (gx.addVertex 3).1
  gx := (gx.addVertex 4).1
  gx := (gx.addVertex 5).1
  gx := (gx.addVertex 6).1
  gx := (gx.addVertex 7).1
  gx := (gx.addVertex 8).1
  gx := (gx.addVertex 9).1
  gx := (gx.addVertex 10).1
  gx := (gx.addVertex 11).1
  gx := (gx.addVertex 12).1
  gx := (gx.addVertex 13).1
  gx := (gx.addVertex 14).1
  gx := gx.addEdgeById 0 1
  gx := gx.addEdgeById 0 2
  gx := gx.addEdgeById 1 3
  gx := gx.addEdgeById 1 4
  gx := gx.addEdgeById 3 7
  gx := gx.addEdgeById 3 8
  gx := gx.addEdgeById 4 9
  gx := gx.addEdgeById 4 10
  gx := gx.addEdgeById 2 5
  gx := gx.addEdgeById 2 6
  gx := gx.addEdgeById 5 11
  gx := gx.addEdgeById 5 12
  gx := gx.addEdgeById 6 13
  gx := gx.addEdgeById 6 14
  gx

def exampleGraph5 : Graph.UndirectedGraph Nat := do
  let mut ug : Graph.UndirectedGraph Nat := ⟨⟨#[]⟩⟩
  ug := (ug.addVertex 0).1
  ug := (ug.addVertex 1).1
  ug := ug.addEdgeById 0 1 5
  ug := ug.addEdgeById 1 0 3
  ug

def exampleGraph6 : Graph Nat := do
  let mut g : Graph Nat := Graph.empty
  g := (g.addVertex 0).1
  g := (g.addVertex 1).1
  g := g.addEdgeById 0 1 1
  g := g.addEdgeById 0 1 2
  g := g.addEdgeById 1 0 3
  g := g.addEdgeById 1 0 4
  g

def exampleGraph7 : Graph.UndirectedGraph Nat := do
  let mut ug : Graph.UndirectedGraph Nat := Graph.UndirectedGraph.empty
  ug := (ug.addVertex 0).1
  ug := (ug.addVertex 1).1
  ug := (ug.addVertex 2).1
  ug := (ug.addVertex 3).1
  ug := ug.addEdgeById 0 1 6
  ug := ug.addEdgeById 0 0 8
  ug := ug.addEdgeById 0 1 14
  ug := ug.addEdgeById 0 2 2
  ug := ug.addEdgeById 1 2 1
  ug := ug.addEdgeById 1 3 3
  ug := ug.addEdgeById 2 3 2
  ug := ug.addEdgeById 3 3 5
  ug

def exampleGraph8 : Graph Nat := do
  let mut gx : Graph Nat := Graph.empty
  gx := (gx.addVertex 0).1
  gx := (gx.addVertex 1).1
  gx := (gx.addVertex 2).1
  gx := (gx.addVertex 3).1
  gx := (gx.addVertex 4).1
  gx := (gx.addVertex 5).1
  gx := (gx.addVertex 6).1
  gx := (gx.addVertex 7).1
  gx := (gx.addVertex 8).1
  gx := (gx.addVertex 9).1
  gx := gx.addEdgeById 0 1
  gx := gx.addEdgeById 1 2
  gx := gx.addEdgeById 1 5
  gx := gx.addEdgeById 1 4
  gx := gx.addEdgeById 1 7
  gx := gx.addEdgeById 2 3
  gx := gx.addEdgeById 2 6
  gx := gx.addEdgeById 3 9
  gx := gx.addEdgeById 3 8
  gx := gx.addEdgeById 6 9
  gx := gx.addEdgeById 6 8
  gx := gx.addEdgeById 5 8
  gx := gx.addEdgeById 7 8
  gx

def printOne : String := toString (exampleGraph4.depthFirstTraversalOrder 0)
def printTwo : String := toString (exampleGraph4.breadthFirstTraversalOrder 0)

def main : IO Unit :=
  IO.println (printOne ++ "\n\n" ++ printTwo)