import Std.Data.Queue
import Std.Data.Stack

/-!
## Container

This is an internal file that as user you do not need to be concerned with, it implement the `Container` used in `Graph.TraversalDeprecated`.
-/

namespace Graph namespace Internal

universe u v

structure Container (β : Type u) (γ : Type v) where
  container : γ
  addFun : β -> γ -> γ
  addAllFun : [Inhabited β] -> Array β -> γ -> γ
  removeFun : γ -> Option (β × γ)

namespace Container

def add (cont : Container β γ) (x : β) : Container β γ := {
  cont with container := cont.addFun x cont.container
}

def addAll [Inhabited β] (cont : Container β γ) (arr : Array β) : Container β γ := {
  cont with container := cont.addAllFun arr cont.container
}

def remove? (cont : Container β γ) : Option (β × (Container β γ)) := match cont.removeFun cont.container with
  | some (element, containerWithoutElement) =>
    let newCont := { cont with container := containerWithoutElement }
    some (element, newCont)
  | none => none

private def addAllQueue (arr : Array β) (cont : Std.Queue β) : Std.Queue β := do
  let mut res := cont
  for e in arr do res := res.enqueue e
  res

private def addAllStack [Inhabited β] (arr : Array β) (cont : Std.Stack β) : Std.Stack β:= do
  let mut res := cont
  for i in [0:arr.size] do res := res.push arr[arr.size-1-i] -- Add in reverse order to make sure that successors of a vertex are chosen in order of edges added
  res

private def pop? {α : Type} [Inhabited α] (s : Std.Stack α) : Option (α × (Std.Stack α)) := match s.peek? with
  | some element => (element, s.pop)
  | none => none

def emptyStack [Inhabited α] : Container α (Std.Stack α) := { container := Std.Stack.empty, addFun := Std.Stack.push, addAllFun := addAllStack, removeFun := pop? }

def emptyQueue : Container α (Std.Queue α) := { container := Std.Queue.empty, addFun := Std.Queue.enqueue, addAllFun := addAllQueue, removeFun := Std.Queue.dequeue? }

instance [ToString α] : ToString (Std.Stack α) where toString s := toString s.vals
instance [ToString γ] : ToString (Container β γ) where toString c := toString c.container

end Container

-- -- Tests:
-- -- Stack
-- def containerTesting1 : Array Nat := do
--   let mut container := Container.emptyStack
--   container := container.add 1
--   container := container.add 2
--   container := container.add 3
--   container := container.add 4
--   container := container.add 5
--   container := container.add 6
--   container := container.add 7
--   container := container.add 8

--   let mut arr : Array Nat := #[]
--   let mut e : Nat := default

--   (e, container) := match container.remove? with
--     | some x => x
--     | none => (42, Container.emptyStack)
--   arr := arr.push e
--   (e, container) := match container.remove? with
--     | some x => x
--     | none => (42, Container.emptyStack)
--   arr := arr.push e
--   (e, container) := match container.remove? with
--     | some x => x
--     | none => (42, Container.emptyStack)
--   arr := arr.push e
--   (e, container) := match container.remove? with
--     | some x => x
--     | none => (42, Container.emptyStack)
--   arr := arr.push e
--   (e, container) := match container.remove? with
--     | some x => x
--     | none => (42, Container.emptyStack)
--   arr := arr.push e
--   (e, container) := match container.remove? with
--     | some x => x
--     | none => (42, Container.emptyStack)
--   arr := arr.push e
--   (e, container) := match container.remove? with
--     | some x => x
--     | none => (42, Container.emptyStack)
--   arr := arr.push e
--   (e, container) := match container.remove? with
--     | some x => x
--     | none => (42, Container.emptyStack)
--   arr := arr.push e

--   arr

-- -- Queue
-- def containerTesting2 : Array Nat := do
--   let mut container := Container.emptyQueue
--   container := container.add 1
--   container := container.add 2
--   container := container.add 3
--   container := container.add 4
--   container := container.add 5
--   container := container.add 6
--   container := container.add 7
--   container := container.add 8

--   let mut arr : Array Nat := #[]
--   let mut e : Nat := default

--   (e, container) := match container.remove? with
--     | some x => x
--     | none => (42, Container.emptyQueue)
--   arr := arr.push e
--   (e, container) := match container.remove? with
--     | some x => x
--     | none => (42, Container.emptyQueue)
--   arr := arr.push e
--   (e, container) := match container.remove? with
--     | some x => x
--     | none => (42, Container.emptyQueue)
--   arr := arr.push e
--   (e, container) := match container.remove? with
--     | some x => x
--     | none => (42, Container.emptyQueue)
--   arr := arr.push e
--   (e, container) := match container.remove? with
--     | some x => x
--     | none => (42, Container.emptyQueue)
--   arr := arr.push e
--   (e, container) := match container.remove? with
--     | some x => x
--     | none => (42, Container.emptyQueue)
--   arr := arr.push e
--   (e, container) := match container.remove? with
--     | some x => x
--     | none => (42, Container.emptyQueue)
--   arr := arr.push e
--   (e, container) := match container.remove? with
--     | some x => x
--     | none => (42, Container.emptyQueue)
--   arr := arr.push e

--   arr

end Internal end Graph
