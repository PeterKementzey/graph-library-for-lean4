import Lake
open Lake DSL

package «Graph» where

lean_lib Graph

--@[default_target]
--lean_exe Graph {
--  root := `Graph
--}

require std from git "https://github.com/leanprover/std4"@"main"
