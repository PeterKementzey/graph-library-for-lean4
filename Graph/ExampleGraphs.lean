import Graph.Graph
import Graph.UndirectedGraph

/-!
## Example graphs

*Note that this module in not imported by default through `import Graph`, import it explicitly using `import Graph.ExampleGraphs`.*

These are just a few simple graphs you can use to play around with the library and see a few ways of constructing a graph. Some of them are `UndirectedGraph`s.
-/

def emptyNatGraph : Graph Nat Nat := Graph.empty

open Graph

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

def exampleGraph3 : Graph Nat Nat := do
  let mut gx : Graph Nat Nat := ⟨#[]⟩
  gx := (gx.addVertex 0).1
  gx := (gx.addVertex 1).1
  gx := (gx.addVertex 2).1
  gx := (gx.addVertex 3).1
  gx := (gx.addVertex 4).1
  gx := gx.addEdgeByID 0 1 2
  gx := gx.addEdgeByID 0 2 5
  gx := gx.addEdgeByID 1 2 1
  gx := gx.addEdgeByID 1 1 1
  gx := gx.addEdgeByID 3 4 5
  gx := gx.addEdgeByID 4 3 9
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
  gx := gx.addEdgeByID 0 1 1
  gx := gx.addEdgeByID 0 2 1
  gx := gx.addEdgeByID 1 3 1
  gx := gx.addEdgeByID 1 4 1
  gx := gx.addEdgeByID 3 7 1
  gx := gx.addEdgeByID 3 8 1
  gx := gx.addEdgeByID 4 9 1
  gx := gx.addEdgeByID 4 10 1
  gx := gx.addEdgeByID 2 5 1
  gx := gx.addEdgeByID 2 6 1
  gx := gx.addEdgeByID 5 11 1
  gx := gx.addEdgeByID 5 12 1
  gx := gx.addEdgeByID 6 13 1
  gx := gx.addEdgeByID 6 14 1
  gx

def exampleGraph5 : Graph.UndirectedGraph Nat Nat := do
  let mut ug : Graph.UndirectedGraph Nat Nat := ⟨⟨#[]⟩⟩
  ug := (ug.addVertex 0).1
  ug := (ug.addVertex 1).1
  ug := ug.addEdgeByID 0 1 5
  ug := ug.addEdgeByID 1 0 3
  ug

def exampleGraph6 : Graph Nat Nat := do
  let mut g : Graph Nat Nat := Graph.empty
  g := (g.addVertex 0).1
  g := (g.addVertex 1).1
  g := g.addEdgeByID 0 1 1
  g := g.addEdgeByID 0 1 2
  g := g.addEdgeByID 1 0 3
  g := g.addEdgeByID 1 0 4
  g

def exampleGraph7 : Graph.UndirectedGraph Nat Nat := do
  let mut ug : Graph.UndirectedGraph Nat Nat := Graph.UndirectedGraph.empty
  ug := (ug.addVertex 0).1
  ug := (ug.addVertex 1).1
  ug := (ug.addVertex 2).1
  ug := (ug.addVertex 3).1
  ug := ug.addEdgeByID 0 0 8
  ug := ug.addEdgeByID 0 1 6
  ug := ug.addEdgeByID 0 1 14
  ug := ug.addEdgeByID 0 2 2
  ug := ug.addEdgeByID 1 2 1
  ug := ug.addEdgeByID 1 3 3
  ug := ug.addEdgeByID 2 3 2
  ug := ug.addEdgeByID 3 3 5
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
  gx := gx.addEdgeByID 0 1 1
  gx := gx.addEdgeByID 1 2 1
  gx := gx.addEdgeByID 1 5 1
  gx := gx.addEdgeByID 1 4 1
  gx := gx.addEdgeByID 1 7 1
  gx := gx.addEdgeByID 2 3 1
  gx := gx.addEdgeByID 2 6 1
  gx := gx.addEdgeByID 3 9 1
  gx := gx.addEdgeByID 3 8 1
  gx := gx.addEdgeByID 6 9 1
  gx := gx.addEdgeByID 6 8 1
  gx := gx.addEdgeByID 5 8 1
  gx := gx.addEdgeByID 7 8 1
  -- gx := (gx.addVertex 10).1
  -- gx := (gx.addVertex 11).1
  -- gx := (gx.addVertex 12).1
  -- gx := (gx.addVertex 13).1
  -- gx := (gx.addVertex 14).1
  -- gx := (gx.addVertex 15).1
  -- gx := (gx.addVertex 16).1
  -- gx := (gx.addVertex 17).1
  -- gx := (gx.addVertex 18).1
  -- gx := (gx.addVertex 19).1
  -- gx := gx.addEdgeByID 10 11 1
  -- gx := gx.addEdgeByID 11 12 1
  -- gx := gx.addEdgeByID 11 15 1
  -- gx := gx.addEdgeByID 11 14 1
  -- gx := gx.addEdgeByID 11 17 1
  -- gx := gx.addEdgeByID 12 13 1
  -- gx := gx.addEdgeByID 12 16 1
  -- gx := gx.addEdgeByID 13 19 1
  -- gx := gx.addEdgeByID 13 18 1
  -- gx := gx.addEdgeByID 16 19 1
  -- gx := gx.addEdgeByID 16 18 1
  -- gx := gx.addEdgeByID 15 18 1
  -- gx := gx.addEdgeByID 17 18 1
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
  gx := gx.addEdgeByID 0 1 43
  gx := gx.addEdgeByID 1 2 40
  gx := gx.addEdgeByID 1 5 5
  gx := gx.addEdgeByID 1 4 10
  gx := gx.addEdgeByID 1 7 4
  gx := gx.addEdgeByID 2 3 13
  gx := gx.addEdgeByID 2 6 10
  gx := gx.addEdgeByID 3 9 100
  gx := gx.addEdgeByID 3 8 20
  gx := gx.addEdgeByID 6 9 100
  gx := gx.addEdgeByID 6 8 5
  gx := gx.addEdgeByID 5 8 4
  gx := gx.addEdgeByID 7 8 2
  gx := gx.addEdgeByID 6 3 10
  gx

def exampleGraph11 : Graph Nat Nat := do
  let mut gx : Graph Nat Nat := Graph.empty
  gx := (gx.addVertex 0).1
  gx := (gx.addVertex 1).1
  gx := (gx.addVertex 2).1
  gx := (gx.addVertex 3).1
  gx := (gx.addVertex 4).1
  gx := (gx.addVertex 5).1
  gx := gx.addEdgeByID 0 1 16
  gx := gx.addEdgeByID 0 2 13
  gx := gx.addEdgeByID 1 2 10
  gx := gx.addEdgeByID 2 1 4
  gx := gx.addEdgeByID 1 3 12
  gx := gx.addEdgeByID 2 4 14
  gx := gx.addEdgeByID 3 2 9
  gx := gx.addEdgeByID 4 3 7
  gx := gx.addEdgeByID 3 5 20
  gx := gx.addEdgeByID 4 5 4
  gx

def exampleGraph11' :=
  let gx : Graph Nat Nat := exampleGraph11.removeAllEdgesFromTo 1 2
  let gx := (gx.addVertex 6).1
  let gx := gx.addEdgeByID 1 6 10
  let gx := gx.addEdgeByID 6 2 10
  gx

def exampleLineGraph :=
  let gx : Graph Nat Nat := Graph.empty
  let gx := (gx.addVertex 0).1
  let gx := (gx.addVertex 1).1
  let gx := (gx.addVertex 2).1
  let gx := (gx.addVertex 3).1
  let gx := (gx.addVertex 4).1
  let gx := gx.addEdgeByID 0 1 1
  let gx := gx.addEdgeByID 1 2 1
  let gx := gx.addEdgeByID 2 3 1
  let gx := gx.addEdgeByID 3 4 1
  gx