import Graph.Dijkstra
import Graph.Graph
import Graph.MaximumFlow
import Graph.MinimumSpanningTree
import Graph.Search
import Graph.TopologicalSort
import Graph.Traverse
import Graph.UndirectedGraph


-- import Graph.Parser
-- import Graph.ExampleGraphs
-- import Graph.TraverseExample

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


-- FIXME depthFirstTraversalOrder original does not add first node removal in case of exampleGraph4

-- def main (argv : List String) : IO Unit := do
--   IO.println exampleGraph5.edgeCount
