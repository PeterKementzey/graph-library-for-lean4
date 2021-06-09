import Graph.All

import Graph.ExampleGraphs
import Graph.Parser
import Graph.TraverseExample
-- This is a comment

/-
this is
a multi line comment
-/

/-!
this is header documentation
# heading
-/


/--
this is function documentation
`code`
# heading
-/
def randomNecessaryFunctionForComment := 5




-- def printOne : String := toString (exampleGraph4.depthFirstPathSearch 0 10)
-- def printTwo : String := toString (exampleGraph4.depthFirstPathSearch 2 10)

-- def euMails := "../stanford-graphs/email-EuAll.txt"

-- def gnutella8 := "../stanford-graphs/p2p-Gnutella08.txt"

-- def topSortGraph := "../stanford-graphs/topsort-gen.txt"
-- def bugSearchGraph := "../stanford-graphs/bug-search.txt"
-- def topSortBugGraph := "../stanford-graphs/bug.txt"

-- def mediumSparse := "../stanford-graphs/medium-sized-sparse-topsort-gen.txt"
-- def maximumWorkingSize := "../stanford-graphs/maximum-working-size-topsort-gen.txt"

-- def main : IO Unit := do

--   let filePath := maximumWorkingSize

--   let graph <- parseGraphFromEdgeList filePath
--   -- IO.println (graph)
--   IO.println (graph.topSort)
--   -- IO.println (graph.depthFirstTraversalOrderWithLeaving 0)


def bitcoinSize := "../generated-graphs/stanford-bitcoin-sized-topsort-gen.txt"
def maximumSize := "../benchmarking/generated-graphs/maximum-working-size-topsort-gen.txt"
def mediumDense := "../generated-graphs/medium-dense-topsort-gen.txt"
def mediumVeryDense := "../generated-graphs/medium-very-dense-topsort-gen.txt"
def mediumSparse := "../generated-graphs/medium-sparse-topsort-gen.txt"
def mediumVerySparse := "../generated-graphs/medium-very-sparse-topsort-gen.txt"
def smallDense := "../generated-graphs/small-dense-topsort-gen.txt"
def smallSparse := "../generated-graphs/small-sparse-topsort-gen.txt"

def main (argv : List String) : IO Unit := do

  IO.println argv
  let filePath := maximumSize

  let graph <- parseGraphFromEdgeList filePath
  -- IO.println (graph)
  -- IO.println (graph.depthFirstTraversalOrderWithLeaving 0)
  timeit "message: " do
    IO.sleep 100
    -- IO.println (sorted.get![0])