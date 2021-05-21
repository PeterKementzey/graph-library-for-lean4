import Std.Data.Queue
import Std.Data.Stack

import Graph.StdExtensions -- uses Std.Data.Stack.pop?


namespace Graph namespace Internal

universes u v

structure Container (β : Type u) (χ : Type v) where
  container : χ
  addFun : β -> χ -> χ
  removeFun : χ -> Option (β × χ)

namespace Container

def add (cont : Container b c) (x : b) : Container b c := {
  cont with container := cont.addFun x cont.container
}

def addAll (cont : Container b c) (arr : Array b) : Container b c := do
  let mut res := cont
  for e in arr do res := res.add e
  res

def remove? (cont : Container b c) : Option (b × (Container b c)) := match cont.removeFun cont.container with
  | some (element, containerWithoutElement) =>
    let newCont := { cont with container := containerWithoutElement }
    some (element, newCont)
  | none => none

def emptyStack [Inhabited α] : Container α (Std.Stack α) := { container := Std.Stack.empty, addFun := Std.Stack.push, removeFun := Std.Stack.pop? }

def emptyQueue {α : Type} : Container α (Std.Queue α) := { container := Std.Queue.empty, addFun := Std.Queue.enqueue, removeFun := Std.Queue.dequeue? }

end Container


-- Tests:
-- Stack
def containerTesting1 : Array Nat := do
  let mut container := Container.emptyStack
  container := container.add 1
  container := container.add 2
  container := container.add 3
  container := container.add 4
  container := container.add 5
  container := container.add 6
  container := container.add 7
  container := container.add 8

  let mut arr : Array Nat := #[]
  let mut e : Nat := arbitrary

  (e, container) := match container.remove? with
    | some x => x
    | none => (42, Container.emptyStack)
  arr := arr.push e
  (e, container) := match container.remove? with
    | some x => x
    | none => (42, Container.emptyStack)
  arr := arr.push e
  (e, container) := match container.remove? with
    | some x => x
    | none => (42, Container.emptyStack)
  arr := arr.push e
  (e, container) := match container.remove? with
    | some x => x
    | none => (42, Container.emptyStack)
  arr := arr.push e
  (e, container) := match container.remove? with
    | some x => x
    | none => (42, Container.emptyStack)
  arr := arr.push e
  (e, container) := match container.remove? with
    | some x => x
    | none => (42, Container.emptyStack)
  arr := arr.push e
  (e, container) := match container.remove? with
    | some x => x
    | none => (42, Container.emptyStack)
  arr := arr.push e
  (e, container) := match container.remove? with
    | some x => x
    | none => (42, Container.emptyStack)
  arr := arr.push e

  arr

-- Queue
def containerTesting2 : Array Nat := do
  let mut container := Container.emptyQueue
  container := container.add 1
  container := container.add 2
  container := container.add 3
  container := container.add 4
  container := container.add 5
  container := container.add 6
  container := container.add 7
  container := container.add 8

  let mut arr : Array Nat := #[]
  let mut e : Nat := arbitrary

  (e, container) := match container.remove? with
    | some x => x
    | none => (42, Container.emptyQueue)
  arr := arr.push e
  (e, container) := match container.remove? with
    | some x => x
    | none => (42, Container.emptyQueue)
  arr := arr.push e
  (e, container) := match container.remove? with
    | some x => x
    | none => (42, Container.emptyQueue)
  arr := arr.push e
  (e, container) := match container.remove? with
    | some x => x
    | none => (42, Container.emptyQueue)
  arr := arr.push e
  (e, container) := match container.remove? with
    | some x => x
    | none => (42, Container.emptyQueue)
  arr := arr.push e
  (e, container) := match container.remove? with
    | some x => x
    | none => (42, Container.emptyQueue)
  arr := arr.push e
  (e, container) := match container.remove? with
    | some x => x
    | none => (42, Container.emptyQueue)
  arr := arr.push e
  (e, container) := match container.remove? with
    | some x => x
    | none => (42, Container.emptyQueue)
  arr := arr.push e

  arr

end Internal end Graph