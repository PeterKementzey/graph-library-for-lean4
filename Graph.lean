import Graph.Dijkstra
import Graph.Graph
import Graph.MaximumFlow
import Graph.MinimumSpanningTree
import Graph.Path
import Graph.Search
import Graph.TopologicalSort
import Graph.Traverse
import Graph.UndirectedGraph

import Graph.Parser
import Graph.ExampleGraphs
import Graph.TraverseExample
-- import Graph.TraverseDeprecated

-- def bitcoinSize := "../benchmarking/generated-graphs/stanford-bitcoin-sized-topsort-gen.txt"
-- def maximumSize := "../benchmarking/generated-graphs/maximum-working-size-topsort-gen.txt"
-- def mediumDense := "../benchmarking/generated-graphs/medium-dense-topsort-gen.txt"
-- def mediumVeryDense := "../benchmarking/generated-graphs/medium-very-dense-topsort-gen.txt"
-- def mediumSparse := "../benchmarking/generated-graphs/medium-sparse-topsort-gen.txt"
-- def mediumVerySparse := "../benchmarking/generated-graphs/medium-very-sparse-topsort-gen.txt"
-- def smallDense := "../benchmarking/generated-graphs/small-dense-topsort-gen.txt"
-- def smallSparse := "../benchmarking/generated-graphs/small-sparse-topsort-gen.txt"
-- def huge := "../benchmarking/generated-graphs/huge-topsort-gen.txt"
-- def testGraph := "../benchmarking/generated-graphs/test-topsort-gen.txt"

def main (argv : List String) : IO Unit := do
  IO.println (exampleGraph4.breadthFirstTraversalOrder #[0])


-- /-- This function ensures that the expression does not get droppen by compiler optimizations. -/
-- def evaluate [ToString α] (expression : α) : IO Unit := IO.FS.writeFile "/dev/null" (toString expression)

-- def benchmarkParsing (filePath : String) : IO (Graph Bool Nat) := do
--   let initial <- IO.monoMsNow
--   let input <- IO.FS.lines filePath
--   let (nodeCount, edgeList) := parseEdgeList input
--   let start <- IO.monoMsNow
--   let graph <- parse nodeCount edgeList
--   let stop <- IO.monoMsNow
--   IO.println ("Read from file and converted to nat in: " ++ (toString (start - initial)) ++ " ms")
--   IO.println ("Parsed graph in: " ++ (toString (stop - start)) ++ " ms")
--   graph

-- def benchmarkTopSort (graph : Graph Bool Nat) : IO Unit := do
--   let start <- IO.monoMsNow
--   let res <- graph.topSortUnsafe
--   let stop <- IO.monoMsNow
--   IO.println ("Sorted graph in: " ++ (toString (stop - start)) ++ " ms")
--   evaluate res
--   -- IO.println (res.get!.back)

-- def benchmarkTopSortSafe (graph : Graph Bool Nat) : IO Unit := do
--   let start <- IO.monoMsNow
--   let res <- graph.topSort
--   let stop <- IO.monoMsNow
--   IO.println ("Safely sorted graph in: " ++ (toString (stop - start)) ++ " ms")
--   evaluate res
--   -- IO.println (res.get!.back)



-- def benchmarkBFS (graph : Graph Bool Nat) : IO Unit := do
--   let start <- IO.monoMsNow
--   let res <- graph.breadthFirstCompleteTraversalOrder
--   let stop <- IO.monoMsNow
--   IO.println ("BFS in: " ++ (toString (stop - start)) ++ " ms")
--   evaluate res

-- def benchmarkBFSOrig (graph : Graph Bool Nat) : IO Unit := do
--   let start <- IO.monoMsNow
--   let res <- graph.breadthFirstCompleteTraversalOrderOrig
--   let stop <- IO.monoMsNow
--   IO.println ("Orig in: " ++ (toString (stop - start)) ++ " ms")
--   evaluate res


-- def main (argv : List String) : IO Unit := do

--   let filePath := testGraph
  
--   let graph <- benchmarkParsing filePath
--   benchmarkBFS graph
--   benchmarkBFSOrig graph
