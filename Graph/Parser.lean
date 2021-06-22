import Graph.Graph

private def toNatPairs (arr : Array String) : Array (Nat × Nat) :=
  let split := arr.map (λ e => (e.split (.=' ')).toArray)
  split.map (λ tup => (
    match tup[0].toNat? with
      | some num => num
      | none => panic! "error on line: " ++ (toString tup),
    match tup[1].toNat? with
      | some num => num
      | none => panic! "error on line: " ++ (toString tup)
    )
  )

-- These functions should be private in general, they are made available for benchmarking purposes
def parseEdgeList (input : Array String) : Nat × Array (Nat × Nat) :=
  let nodeCount : Nat := match input[0].toNat? with
    | some n => n
    | none => panic! "Node count not a natural number: " ++ input[0]
  let edgeList := toNatPairs (input.eraseIdx 0)
  (nodeCount, edgeList)

-- These functions should be private in general, they are made available for benchmarking purposes
def parse (nodeCount : Nat) (edges : Array (Nat × Nat)) : Graph Bool Nat := do
  let mut gx : Graph Bool Nat := Graph.makeGraphFromArray (mkArray nodeCount false)
  -- let mut i := 1
  for edge in edges do
    gx := gx.addEdgeByID edge.1 edge.2 0 -- i
    -- i := i+1
  gx

/-!
## Parser

*Note that this module in not imported by default through `import Graph`, import it explicitly using `import Graph.Parser`.*
-/

/-- Parses a graph from an edge list, first element should contain node count, rest of the lines should be an edge list of vertex IDs where "0 1\n" means edge from ID 0 to 1. -/
def parseGraphFromEdgeList (input : Array String) : Graph Bool Nat :=
  let (nodeCount, edgeList) := parseEdgeList input
  parse nodeCount edgeList

/-- See parseGraphFromEdgeList for description of format. -/
def parseGraphFromEdgeListFile (filePath : System.FilePath) : IO (Graph Bool Nat) := do
  let input <- IO.FS.lines filePath
  parseGraphFromEdgeList input