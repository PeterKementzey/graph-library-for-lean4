import Std.Data.Stack
import Std.Data.HashSet

namespace Std

-- TODO put this in graph.internal.std.option, then open graph.internal and it might work with dot notation
namespace Option

def get! {α : Type _} [Inhabited α] : Option α -> α
  | some x => x
  | none => panic! "This cannot be none"

end Option


namespace HashSet

def merge {α : Type u} [BEq α] [Hashable α] (l : HashSet α) (r : HashSet α) : HashSet α := r.fold insert l

end HashSet


namespace Stack

def pop? {α : Type} [Inhabited α] (s : Std.Stack α) : Option (α × (Std.Stack α)) := match s.peek? with
  | some element => (element, s.pop)
  | none => none

end Stack


end Std