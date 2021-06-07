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
def hi := 5




-- def printOne : String := toString (exampleGraph4.depthFirstPathSearch 0 10)
-- def printTwo : String := toString (exampleGraph4.depthFirstPathSearch 2 10)

def euMails := (265214, "../stanford-graphs/email-EuAll.txt")

def gnutella8 := (6301, "../stanford-graphs/p2p-Gnutella08.txt")

def topSortGraph := (20, "../stanford-graphs/topsort-gen.txt")
def bugSearchGraph := (5, "../stanford-graphs/bug-search.txt")
def topSortBugGraph := (21, "../stanford-graphs/bug.txt")

def main : IO Unit := do

  let (nodeCount, filePath) := topSortGraph

  let graph <- parseGraphFromEdgeList nodeCount filePath
  -- IO.println (graph)
  IO.println (graph.topSort)
  -- IO.println (graph.depthFirstTraversalOrderWithLeaving 0)