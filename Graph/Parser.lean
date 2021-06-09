import Graph.Graph

def toNatPairs (arr : Array String) : Array (Nat × Nat) :=
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


def parser (nodeCount : Nat) (input : Array String) : Graph Bool Nat := do
  let edges : Array (Nat × Nat) := toNatPairs input
  let mut gx : Graph Bool Nat := Graph.makeGraphFromArray (mkArray nodeCount false)
  -- let mut i := 1
  for edge in edges do
    gx := gx.addEdgeById edge.1 edge.2 0 -- i
    -- i := i+1
  gx

def parseGraphFromEdgeList (filePath : System.FilePath) : IO (Graph Bool Nat) := do
  let input <- IO.FS.lines filePath
  let nodeCount : Nat := match input[0].toNat? with
    | some n => n
    | none => panic! "Node count not a natural number: " ++ input[0]
  parser nodeCount (input.eraseIdx 0)