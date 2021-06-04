import Graph.All

import Graph.ExampleGraphs

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






def printOne : String := toString (exampleGraph4.depthFirstPathSearch 0 10)
def printTwo : String := toString (exampleGraph4.depthFirstPathSearch 2 10)

def main : IO Unit :=
  IO.println (printOne ++ "\n\n" ++ printTwo)