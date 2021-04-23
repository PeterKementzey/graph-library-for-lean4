import Graph.graphrepresentation
import Std.Data.Queue
import Std.Data.Stack

namespace Std
namespace Stack

def pop? {α : Type} [Inhabited α] (s : Std.Stack α) : Option (α × (Std.Stack α)) := match s.peek? with
  | some element => (element, s.pop)
  | none => none

end Stack
end Std


namespace Graph
variable {α : Type} [BEq α] [Inhabited α]
universe u

structure Container (β : Type u) (containerType : Type u -> Type u) where -- TODO make private
  χ := containerType β
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

def emptyStack [Inhabited α] : Container α Std.Stack := { container := Std.Stack.empty, addFun := Std.Stack.push, removeFun := Std.Stack.pop? }

def emptyQueue {α : Type} : Container α Std.Queue := { container := Std.Queue.empty, addFun := Std.Queue.enqueue, removeFun := Std.Queue.dequeue? }

end Container
-- Note: See test functions for Container at the end of this file


-- private def BFSAuxx (g : Graph α) (target : Nat) (visited : Array Bool) (cont : Container Nat β) : Nat -> Bool
--   | 0 => false
--   | n + 1 => do
--     let mut container : Container Nat β := cont
--     let mut visitedMutable := visited
--     match container.remove? with
--       | none => false
--       | some (current, container) =>
--         for edge in g.vertices[current].adjacencyList do
--           if !visitedMutable[edge.target] then
--             if edge.target == target then return true else
--             visitedMutable := visitedMutable.set! edge.target true
--             container := container.add edge.target
--         BFSAuxx g target visitedMutable container n

private def BFSAux (g : Graph α) (target : Nat) (visited : Array Bool) (container : Container Nat β) : Nat -> Bool
  | 0 => false
  | n+1 => match container.remove? with
    | none => false
    | some (current, cont) => do
      let mut visitedMut := visited
      let unvisitedNeighborIds := (g.vertices[current].adjacencyList.map (λ e => e.target)).filter (λ e => !visitedMut[e])
      if unvisitedNeighborIds.contains target then true
      else
        for neighbor in unvisitedNeighborIds do
          visitedMut := visitedMut.set! neighbor true
        let containerWithNewNodes := container.addAll unvisitedNeighborIds
        BFSAux g target visitedMut containerWithNewNodes n
        

def breadthFirstSearch (g : Graph α) (source : Nat) (target : Nat) : Bool := 
  if source == target then true
  else 
    let visited : Array Bool := mkArray g.vertices.size false
    BFSAux g target (visited.set! source true) (Container.emptyQueue.add source) g.vertices.size



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

end Graph