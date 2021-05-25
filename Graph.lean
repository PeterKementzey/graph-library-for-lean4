import Graph.All

def exampleGraph1 : Graph Char Nat :=
  let v0 : Vertex Char Nat := { payload := 'a', adjacencyList := #[ {target := 1, weight := 1} ] }
  let v1 : Vertex Char Nat := { payload := 'b', adjacencyList := #[ {target := 2, weight := 1} ] }
  let v2 : Vertex Char Nat := { payload := 'c', adjacencyList := #[ {target := 3, weight := 1}, {target := 2, weight := 1} ] }
  let v3 : Vertex Char Nat := { payload := 'd', adjacencyList := #[ {target := 0, weight := 1} ] }
  ⟨#[v0, v1, v2, v3]⟩

def exampleGraph2 : Graph Char Nat :=
  let v0 : Vertex Char Nat := {
    payload := '0',
    adjacencyList := #[
      {target := 1, weight := 4},
      {target := 2, weight := 9},
      {target := 3, weight := 6}
    ]
  }
  let v1 : Vertex Char Nat := {
    payload := '1',
    adjacencyList := #[
      {target := 0, weight := 4},
      {target := 2, weight := 4},
      {target := 4, weight := 9}
    ]
  }
  let v2 : Vertex Char Nat := {
    payload := '2',
    adjacencyList := #[
      {target := 0, weight := 9},
      {target := 1, weight := 4},
      {target := 3, weight := 3},
      {target := 4, weight := 1}
    ]
  }
  let v3 : Vertex Char Nat := {
    payload := '3',
    adjacencyList := #[
      {target := 0, weight := 6},
      {target := 2, weight := 3},
      {target := 4, weight := 5}
    ]
  }
  let v4 : Vertex Char Nat := {
    payload := '4',
    adjacencyList := #[
      {target := 1, weight := 9},
      {target := 2, weight := 1},
      {target := 3, weight := 5}
    ]
  }
  ⟨#[v0, v1, v2, v3, v4]⟩

instance : Graph.DefaultEdgeWeight Nat where default := 1

def exampleGraph3 : Graph Nat Nat := do
  let mut gx : Graph Nat Nat := ⟨#[]⟩
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

def exampleGraph4 : Graph Nat Nat := do
  let mut gx : Graph Nat Nat := Graph.empty
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

def exampleGraph5 : Graph.UndirectedGraph Nat Nat := do
  let mut ug : Graph.UndirectedGraph Nat Nat := ⟨⟨#[]⟩⟩
  ug := (ug.addVertex 0).1
  ug := (ug.addVertex 1).1
  ug := ug.addEdgeById 0 1 5
  ug := ug.addEdgeById 1 0 3
  ug

def exampleGraph6 : Graph Nat Nat := do
  let mut g : Graph Nat Nat := Graph.empty
  g := (g.addVertex 0).1
  g := (g.addVertex 1).1
  g := g.addEdgeById 0 1 1
  g := g.addEdgeById 0 1 2
  g := g.addEdgeById 1 0 3
  g := g.addEdgeById 1 0 4
  g

def exampleGraph7 : Graph.UndirectedGraph Nat Nat := do
  let mut ug : Graph.UndirectedGraph Nat Nat := Graph.UndirectedGraph.empty
  ug := (ug.addVertex 0).1
  ug := (ug.addVertex 1).1
  ug := (ug.addVertex 2).1
  ug := (ug.addVertex 3).1
  ug := ug.addEdgeById 0 0 8
  ug := ug.addEdgeById 0 1 6
  ug := ug.addEdgeById 0 1 14
  ug := ug.addEdgeById 0 2 2
  ug := ug.addEdgeById 1 2 1
  ug := ug.addEdgeById 1 3 3
  ug := ug.addEdgeById 2 3 2
  ug := ug.addEdgeById 3 3 5
  ug

def exampleGraph8 : Graph Nat Nat := do
  let mut gx : Graph Nat Nat := Graph.empty
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
  gx := (gx.addVertex 10).1
  gx := (gx.addVertex 11).1
  gx := (gx.addVertex 12).1
  gx := (gx.addVertex 13).1
  gx := (gx.addVertex 14).1
  gx := (gx.addVertex 15).1
  gx := (gx.addVertex 16).1
  gx := (gx.addVertex 17).1
  gx := (gx.addVertex 18).1
  gx := (gx.addVertex 19).1
  gx := gx.addEdgeById 10 11
  gx := gx.addEdgeById 11 12
  gx := gx.addEdgeById 11 15
  gx := gx.addEdgeById 11 14
  gx := gx.addEdgeById 11 17
  gx := gx.addEdgeById 12 13
  gx := gx.addEdgeById 12 16
  gx := gx.addEdgeById 13 19
  gx := gx.addEdgeById 13 18
  gx := gx.addEdgeById 16 19
  gx := gx.addEdgeById 16 18
  gx := gx.addEdgeById 15 18
  gx := gx.addEdgeById 17 18
  gx

def exampleGraph9 : Graph Nat Nat := do -- Graph without edges
  let mut gx : Graph Nat Nat := Graph.empty
  gx := (gx.addVertex 0).1
  gx := (gx.addVertex 1).1
  gx := (gx.addVertex 2).1
  gx := (gx.addVertex 3).1
  gx := (gx.addVertex 4).1
  gx := (gx.addVertex 5).1
  gx

def exampleGraph10 : Graph Nat Nat := do
  let mut gx : Graph Nat Nat := Graph.empty
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
  gx := gx.addEdgeById 0 1 43
  gx := gx.addEdgeById 1 2 40
  gx := gx.addEdgeById 1 5 5
  gx := gx.addEdgeById 1 4 10
  gx := gx.addEdgeById 1 7 4
  gx := gx.addEdgeById 2 3 13
  gx := gx.addEdgeById 2 6 10
  gx := gx.addEdgeById 3 9 100
  gx := gx.addEdgeById 3 8 20
  gx := gx.addEdgeById 6 9 100
  gx := gx.addEdgeById 6 8 5
  gx := gx.addEdgeById 5 8 4
  gx := gx.addEdgeById 7 8 2
  gx := gx.addEdgeById 6 3 10
  gx

private def foldEdges (e : (Edge Graph.MaxFlowEdge)) (s : String) : String :=
  s ++ "   target: " ++ (toString e.target) ++ ", flow: " ++ (toString e.weight.flow) ++ ", capacity: " ++ (toString e.weight.capacity) ++ "\n"

instance : ToString Graph.VertexState where toString s := "Excess: " ++ (toString s.excess) ++ ", height: " ++ (toString s.height) ++ ", next vertex: " ++ (toString s.nextVertex)
  ++ "\ncurrent neighbor: " ++ (toString s.currentNeighbor) ++ ", neighbor list: " ++ (toString s.neighborList)
instance : ToString (Vertex Graph.VertexState Graph.MaxFlowEdge) where toString v := "\nVertex state: " ++ toString v.payload ++ "\n" ++ v.adjacencyList.foldr foldEdges "" ++ "\n"
instance : ToString Graph.FlowNetwork where toString fn := do
  let mut indices : Array Nat := Array.empty
  for i in [0:fn.vertices.size-1] do indices := indices.push i
  toString (indices.zip fn.vertices)

def printOne : String := toString (exampleGraph10)
def printTwo : String := toString ((exampleGraph10.findMaxFlow 0 8).get!)

def main : IO Unit :=
  IO.println (printOne ++ "\n\n" ++ printTwo)