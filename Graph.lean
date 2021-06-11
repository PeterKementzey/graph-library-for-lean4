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


def bitcoinSize := "../benchmarking/generated-graphs/stanford-bitcoin-sized-topsort-gen.txt"
def maximumSize := "../benchmarking/generated-graphs/maximum-working-size-topsort-gen.txt"
def mediumDense := "../benchmarking/generated-graphs/medium-dense-topsort-gen.txt"
def mediumVeryDense := "../benchmarking/generated-graphs/medium-very-dense-topsort-gen.txt"
def mediumSparse := "../benchmarking/generated-graphs/medium-sparse-topsort-gen.txt"
def mediumVerySparse := "../benchmarking/generated-graphs/medium-very-sparse-topsort-gen.txt"
def smallDense := "../benchmarking/generated-graphs/small-dense-topsort-gen.txt"
def smallSparse := "../benchmarking/generated-graphs/small-sparse-topsort-gen.txt"
def huge := "../benchmarking/generated-graphs/huge-topsort-gen.txt"


-- Wrt the `timeit` shenanigans: there could be all kinds of things going
-- on behind the scenes that would make this benchmark fail. E.g. maybe the
-- compiler is floating a `let` out of the `timeit` action, so it doesn't
-- measure anything. To guard against that, you could try to turn the `let
-- x := topsort y` into `let x <- pure (topsort x)`; maybe the compiler is
-- more reluctant to float out an IO action. Maybe also put a `print`
-- behind the `topsort` within the `timeit` to force evaluation at this point.



def main (argv : List String) : IO Unit := do

  let filePath := huge

  let graph <- parseGraphFromEdgeList filePath
  IO.println graph.vertices.back.payload
  IO.println "Parsed graph"
  let start <- IO.monoMsNow
  let res <- graph.topSort
  let stop <- IO.monoMsNow
  IO.println ("Sorted graph in: " ++ (toString (stop - start)) ++ " ms")
  IO.println (res.get![0])
  