import Graph.Graph
import Std.Data.HashSet

namespace Graph

variable {α : Type} [BEq α] [Inhabited α] variable {β : Type}


structure VertexState where
  excess : Nat := 0
  height : Nat := 0
  nextVertex : Nat
  currentNeighbor : Nat := 0
  neighborList : Array Nat

instance : Inhabited VertexState := ⟨ { nextVertex := arbitrary, neighborList := Array.empty } ⟩ 

structure MaxFlowEdge where
  capacity : Nat
  flow : Nat := 0

def FlowNetwork := Graph VertexState MaxFlowEdge -- TODO make private again
private def FlowVertex := Vertex VertexState MaxFlowEdge

instance [Inhabited VertexState] : Inhabited FlowVertex := ⟨ { payload := arbitrary } ⟩ -- Why does this not work automatically?
instance : Inhabited FlowNetwork := ⟨ { } ⟩ -- what the hell

-- variable (FlowNetwork := Graph α Nat) -- TODO how to do this?

-- def basicMinimumCut (g : Graph α β) :

instance : Inhabited MaxFlowEdge := ⟨ { capacity := arbitrary } ⟩ 
instance [Inhabited γ] : Inhabited (Edge γ) := ⟨ 0, arbitrary ⟩

private def createAdjacencyListAndNeighborSets (vertex : Vertex α Nat) (id : Nat) (neighborSets : Array (Std.HashSet Nat)) : Option ((Array (Edge MaxFlowEdge)) × (Array (Std.HashSet Nat))) := do
  let mut neighborSets := neighborSets
  let mut currentNeighborSet := neighborSets[id]
  let mut adjacencyList : Array (Edge MaxFlowEdge) := Array.empty
  for i in [0:vertex.adjacencyList.size] do
    let edge := vertex.adjacencyList[i]
    if currentNeighborSet.contains edge.target then return none else -- There is either an antiparallel edge (see Cormen et al. Introduction to Algorithms) or the graph is not simple
    neighborSets := neighborSets.modify edge.target (λ neighborSet => neighborSet.insert id)
    currentNeighborSet := currentNeighborSet.insert edge.target
    adjacencyList := adjacencyList.push { edge with weight := { capacity := edge.weight, flow := 0 } }

  let resultNeighborSets := neighborSets.set! id currentNeighborSet
  some (adjacencyList, resultNeighborSets)
  

private def nullFlowNetwork (g : Graph α Nat) : Option FlowNetwork := do
  let mut adjacencyLists : Array (Array (Edge MaxFlowEdge)) := Array.empty
  let mut neighborSets : Array (Std.HashSet Nat) := mkArray g.vertices.size Std.HashSet.empty
  let mut nextVertexPointers : Array Nat := Array.empty
  for i in [0:g.vertices.size] do
    match createAdjacencyListAndNeighborSets g.vertices[i] i neighborSets with
      | some (newAdjacencyList, newNeighborSets) =>
        adjacencyLists := adjacencyLists.push newAdjacencyList
        neighborSets := newNeighborSets
        nextVertexPointers := nextVertexPointers.push i
      | none => return none
  let mut vertices : Array FlowVertex := Array.empty
  for i in [0:g.vertices.size] do
    let vertexState : VertexState := {
      nextVertex := nextVertexPointers[i]
      neighborList := neighborSets[i].toArray
    }
    vertices := vertices.push { payload := vertexState, adjacencyList := adjacencyLists[i] }

  some ⟨ vertices ⟩
  
namespace FlowNetwork

private def initializePreflow (flowNetwork : FlowNetwork) (source : Nat) : FlowNetwork :=
  let verticesWithSourceAtHeightAndPreflowMaximized : Array FlowVertex := flowNetwork.vertices.modify source (λ vertex => {
    payload := { vertex.payload with height := flowNetwork.vertices.size } 
    adjacencyList := vertex.adjacencyList.map (λ edge =>
      { edge with weight := { edge.weight with flow := edge.weight.capacity } }
    )
  })

  do
    let mut vertices := verticesWithSourceAtHeightAndPreflowMaximized
    for edge in verticesWithSourceAtHeightAndPreflowMaximized[source].adjacencyList do
      vertices := vertices.modify edge.target (λ vertex =>
        { vertex with payload := { vertex.payload with excess := edge.weight.capacity }}
      )
    ⟨ vertices ⟩


private def residualCapacity (flowNetwork : FlowNetwork) (u : Nat) (v : Nat) : Option Nat := match flowNetwork.vertices[u].adjacencyList.find? (λ edge => edge.target == v) with
  | some edge => edge.weight.capacity - edge.weight.flow
  | none => match flowNetwork.vertices[v].adjacencyList.find? (λ edge => edge.target == u) with
    | some edge => edge.weight.flow
    | none => none

private def updateFlow (flowNetwork : FlowNetwork) (u : Nat) (edgeId : Nat) (newFlow : Nat) : FlowNetwork := ⟨
  flowNetwork.vertices.modify u (λ vertex => { vertex with adjacencyList := vertex.adjacencyList.modify edgeId (λ edge =>
    { edge with weight := { edge.weight with flow := newFlow } }
  )})
⟩

private def updateExcess (flowNetwork : FlowNetwork) (u : Nat) (newExcess : Nat) : FlowNetwork := ⟨
  flowNetwork.vertices.modify u (λ vertex => { vertex with payload :=
    { vertex.payload with excess := newExcess }
  } )
⟩

private def push (flowNetwork : FlowNetwork) (u : Nat) (v : Nat) : FlowNetwork :=
    let excessOfu := flowNetwork.vertices[u].payload.excess
    let quantity : Nat :=
      let residualCapacity := match flowNetwork.residualCapacity u v with | some x => x | none => panic! "Pushing on non existent edge"
      if excessOfu < residualCapacity then excessOfu else residualCapacity
    let udpatedFlow : FlowNetwork := match flowNetwork.vertices[u].adjacencyList.findIdx? (λ edge => edge.target == v) with
      | some edgeId => flowNetwork.updateFlow u edgeId (flowNetwork.vertices[u].adjacencyList[edgeId].weight.flow + quantity)
      | none => match flowNetwork.vertices[v].adjacencyList.findIdx? (λ edge => edge.target == u) with
        | some edgeId => flowNetwork.updateFlow v edgeId (flowNetwork.vertices[v].adjacencyList[edgeId].weight.flow - quantity)
        | none => panic! "Pushing on non existent edge"
    let excessOfv := flowNetwork.vertices[v].payload.excess
    (flowNetwork.updateExcess u (excessOfu - quantity)).updateExcess v (excessOfv + quantity)

private def findLowestNeighborAux (flowNetwork : FlowNetwork) (u : Nat) (neighborList : Array Nat) (current : Nat) (min : Nat) : Nat -> Nat
  | 0 => panic! "This should be impossible"
  | n + 1 =>
    if current >= neighborList.size then min else
    if (flowNetwork.residualCapacity u current).get! == 0 then flowNetwork.findLowestNeighborAux u neighborList (current + 1) min n else
    let nextMin := if flowNetwork.vertices[min].payload.height > flowNetwork.vertices[current].payload.height then current else min
    flowNetwork.findLowestNeighborAux u neighborList (current + 1) nextMin n

private def findLowestNeighbor (flowNetwork : FlowNetwork) (u : Nat) : Nat :=
  let neighborList := flowNetwork.vertices[u].payload.neighborList
  let initial := (neighborList.findIdx? (λ id => (flowNetwork.residualCapacity u id).get! > 0)).get!
  flowNetwork.findLowestNeighborAux u neighborList (initial + 1) initial neighborList.size

private def relabel (flowNetwork : FlowNetwork) (u : Nat) : FlowNetwork :=
  let lowestNeighbor := flowNetwork.findLowestNeighbor u
  let newHeight := flowNetwork.vertices[lowestNeighbor].payload.height + 1
  ⟨ flowNetwork.vertices.modify u (λ vertex => { vertex with payload := { vertex.payload with height := newHeight } } ) ⟩ 

private def upperBoundOfDischargeIterations (flowNetwork : FlowNetwork) (u : Nat) : Nat :=
  let numberOfPointerAdvancements := flowNetwork.vertices[u].payload.neighborList.size
  let numberOfRelabelOperations := numberOfPointerAdvancements
  let numberOfPushOperations := flowNetwork.vertices[u].payload.excess + 1
  numberOfPointerAdvancements + numberOfRelabelOperations + numberOfPushOperations + 1

private def discharge (flowNetwork : FlowNetwork) (u : Nat) : Nat -> FlowNetwork
  | 0 => flowNetwork
  | n + 1 => 
    let vertexState := flowNetwork.vertices[u].payload
    if vertexState.excess == 0 then flowNetwork else
    let newFlowNetwork := if vertexState.currentNeighbor >= vertexState.neighborList.size then
      let flowNetworkWithRelabeling := flowNetwork.relabel u
    ⟨ flowNetworkWithRelabeling.vertices.modify u (λ vertex => { vertex with payload := { vertex.payload with currentNeighbor := 0 } } ) ⟩ 
    else
      let currentNeighborId := vertexState.neighborList[vertexState.currentNeighbor]
      if (vertexState.height == flowNetwork.vertices[currentNeighborId].payload.height + 1) && ((flowNetwork.residualCapacity u currentNeighborId).get! > 0) then
        flowNetwork.push u currentNeighborId
      else
        ⟨ flowNetwork.vertices.modify u (λ vertex => { vertex with payload := { vertex.payload with currentNeighbor := vertex.payload.currentNeighbor + 1 } } ) ⟩
    newFlowNetwork.discharge u n

end FlowNetwork

open FlowNetwork

-- Push-relabel algorithm using relabel-to-fron selection
def findMaxFlow (g : Graph α Nat) (source : Nat) (sink : Nat) : Option FlowNetwork := -- TODO change to this: Option (Graph α Nat) :=
  match nullFlowNetwork g with
    | none => none
    | some initialGraph =>
      let preflowGraph : FlowNetwork := initialGraph.initializePreflow source

      preflowGraph

end Graph
