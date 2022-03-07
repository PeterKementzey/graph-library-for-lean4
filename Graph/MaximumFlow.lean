import Graph.Graph
import Std.Data.HashSet

namespace Graph

variable {α : Type} [Inhabited α] {β : Type}

private structure VertexState where
  excess : Nat := 0
  height : Nat := 0
  nextVertex : Nat
  currentNeighbor : Nat := 0
  neighborList : Array Nat

private instance : Inhabited VertexState := ⟨ { nextVertex := default, neighborList := Array.empty } ⟩ 
-- instance : ToString VertexState where toString s := "Excess: " ++ (toString s.excess) ++ ", height: " ++ (toString s.height) ++ ", next vertex: " ++ (toString s.nextVertex)
--   ++ "\ncurrent neighbor: " ++ (toString s.currentNeighbor) ++ ", neighbor list: " ++ (toString s.neighborList)

structure MaxFlowEdge where
  capacity : Nat
  flow : Nat := 0

instance : ToString MaxFlowEdge where toString mfe := "flow: " ++ (toString mfe.flow) ++ ", capacity: " ++ (toString mfe.capacity)
instance : Inhabited MaxFlowEdge := ⟨ { capacity := default } ⟩ 
private instance [Inhabited γ] : Inhabited (Edge γ) := ⟨ 0, default ⟩

private def FlowNetwork := Graph VertexState MaxFlowEdge

private def FlowVertex := Vertex VertexState MaxFlowEdge
private instance [Inhabited VertexState] : Inhabited FlowVertex := ⟨ { payload := default } ⟩


private def createAdjacencyListAndNeighborSets (vertex : Vertex α Nat) (id : Nat) (neighborSets : Array (Std.HashSet Nat)) : Option ((Array (Edge MaxFlowEdge)) × (Array (Std.HashSet Nat))) := Id.run do
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
  

private def nullFlowNetwork (g : Graph α Nat) : Option FlowNetwork := Id.run do
  let mut adjacencyLists : Array (Array (Edge MaxFlowEdge)) := Array.empty
  let mut neighborSets : Array (Std.HashSet Nat) := mkArray g.vertexCount Std.HashSet.empty
  let mut nextVertexPointers : Array Nat := Array.empty
  for i in g.getAllVertexIDs do
    match createAdjacencyListAndNeighborSets g.vertices[i] i neighborSets with
      | some (newAdjacencyList, newNeighborSets) =>
        adjacencyLists := adjacencyLists.push newAdjacencyList
        neighborSets := newNeighborSets
        nextVertexPointers := nextVertexPointers.push (i + 1)
      | none => return none
  let mut vertices : Array FlowVertex := Array.empty
  for i in g.getAllVertexIDs do
    let vertexState : VertexState := {
      nextVertex := nextVertexPointers[i]
      neighborList := neighborSets[i].toArray
    }
    vertices := vertices.push { payload := vertexState, adjacencyList := adjacencyLists[i] }

  some ⟨ vertices ⟩
  
namespace FlowNetwork

private def initializePreflow (flowNetwork : FlowNetwork) (source : Nat) : FlowNetwork :=
  let verticesWithSourceAtHeightAndPreflowMaximized : Array FlowVertex := flowNetwork.vertices.modify source (λ vertex => {
    payload := { vertex.payload with height := flowNetwork.vertexCount } 
    adjacencyList := vertex.adjacencyList.map (λ edge =>
      { edge with weight := { edge.weight with flow := edge.weight.capacity } }
    )
  })

  Id.run do
    let mut vertices := verticesWithSourceAtHeightAndPreflowMaximized
    for edge in verticesWithSourceAtHeightAndPreflowMaximized[source].adjacencyList do
      vertices := vertices.modify edge.target (λ vertex =>
        { vertex with payload := { vertex.payload with excess := edge.weight.capacity }}
      )
    ⟨ vertices ⟩


private def residualCapacity (flowNetwork : FlowNetwork) (u : Nat) (v : Nat) : Option Nat := match flowNetwork.vertices[u].adjacencyList.find? (λ edge => edge.target == v) with
  | some edge => some (edge.weight.capacity - edge.weight.flow)
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
    let updatedFlow : FlowNetwork := match flowNetwork.vertices[u].adjacencyList.findIdx? (λ edge => edge.target == v) with
      | some edgeId => flowNetwork.updateFlow u edgeId (flowNetwork.vertices[u].adjacencyList[edgeId].weight.flow + quantity)
      | none => match flowNetwork.vertices[v].adjacencyList.findIdx? (λ edge => edge.target == u) with
        | some edgeId => flowNetwork.updateFlow v edgeId (flowNetwork.vertices[v].adjacencyList[edgeId].weight.flow - quantity)
        | none => have : Inhabited FlowNetwork := ⟨ { } ⟩ ; panic! "Pushing on non existent edge"
    let excessOfv := flowNetwork.vertices[v].payload.excess
    (updatedFlow.updateExcess u (excessOfu - quantity)).updateExcess v (excessOfv + quantity)

private def findLowestNeighborAux (flowNetwork : FlowNetwork) (u : Nat) (neighborList : Array Nat) (current : Nat) (min : Nat) : Nat -> Nat
  | 0 => panic! "This should be impossible"
  | n + 1 =>
    if current >= neighborList.size then min else
    if (flowNetwork.residualCapacity u neighborList[current]).get! == 0 then findLowestNeighborAux flowNetwork u neighborList (current + 1) min n else
    let nextMin := if flowNetwork.vertices[neighborList[min]].payload.height > flowNetwork.vertices[neighborList[current]].payload.height then current else min
    findLowestNeighborAux flowNetwork u neighborList (current + 1) nextMin n

private def findLowestNeighbor (flowNetwork : FlowNetwork) (u : Nat) : Nat :=
  let neighborList := flowNetwork.vertices[u].payload.neighborList
  let initial := (neighborList.findIdx? (λ id => (flowNetwork.residualCapacity u id).get! > 0)).get!
  flowNetwork.findLowestNeighborAux u neighborList (initial + 1) initial neighborList.size

private def relabel (flowNetwork : FlowNetwork) (u : Nat) : FlowNetwork :=
  let lowestNeighbor := flowNetwork.findLowestNeighbor u
  let newHeight := flowNetwork.vertices[flowNetwork.vertices[u].payload.neighborList[lowestNeighbor]].payload.height + 1
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
    discharge newFlowNetwork u n

private def removeFromList (flowNetwork : FlowNetwork) (id : Nat) (head : Nat) : FlowNetwork × Nat :=
  let wrapAround n := n % flowNetwork.vertexCount
  if id == head then
    let newList := flowNetwork.vertices.modify id (λ vertex =>
      { vertex with payload := { vertex.payload with nextVertex := flowNetwork.vertexCount } }
    )
    (⟨ newList ⟩, flowNetwork.vertices[head].payload.nextVertex) else
  let parentId : Nat := (flowNetwork.vertices.findIdx? (λ vertex => vertex.payload.nextVertex == id)).get!
  let newList := flowNetwork.vertices.modify parentId (λ vertex =>
    { vertex with payload := { vertex.payload with nextVertex := flowNetwork.vertices[id].payload.nextVertex } }
  )
  (⟨ newList ⟩, head)

private def addToFrontOfList (flowNetwork : FlowNetwork) (id : Nat) (head : Nat) : FlowNetwork × Nat :=
  if id == head then (flowNetwork, head) else
  let newList := flowNetwork.vertices.modify id (λ vertex =>
    { vertex with payload := { vertex.payload with nextVertex := head } }
  )
  (⟨ newList ⟩, id)

private def initializeVertexList (flowNetwork : FlowNetwork) (source : Nat) (sink : Nat) : FlowNetwork × Nat :=
  let wrapAround n := n % (flowNetwork.vertexCount)
  let (sourceSkipped, sourceSkippedHead) := flowNetwork.removeFromList source 0
  let (sinkSkipped, sinkSkippedHead) := sourceSkipped.removeFromList sink sourceSkippedHead
  (sinkSkipped, sinkSkippedHead)

-- Future work: implmenet upper bound on iteration count
private partial def relabelToFront (flowNetwork : FlowNetwork) (current : Nat) (head : Nat) : FlowNetwork :=
  if current >= flowNetwork.vertexCount then flowNetwork else
  let oldHeight := flowNetwork.vertices[current].payload.height
  let newFlowNetwork : FlowNetwork := flowNetwork.discharge current (flowNetwork.upperBoundOfDischargeIterations current)
  let (newList, newHead) : FlowNetwork × Nat := if newFlowNetwork.vertices[current].payload.height > oldHeight then
    let (listWithoutCurrent, headWithoutCurrent) := newFlowNetwork.removeFromList current head
    listWithoutCurrent.addToFrontOfList current headWithoutCurrent
  else
    (newFlowNetwork, head)
  relabelToFront newList (newList.vertices[current].payload.nextVertex) newHead

end FlowNetwork

/-- Calculates a maximum flow in the graph from source to sink using the push-relabel algorithm with the relabel-to-front selection rule.
    Please make sure that the graph does not contain parallel edges or anti-parallel (opposite direction between same nodes) edges,
    as these are not yet supported. Additionally, the graph should not contain self-loops. Currently this algorithm only works for
    graphs with `Nat` edge weights, you may use the `mapEdges` function to map your edges to `Nat` type.The edge weights should represent
    the capacities on the edges. The resulting graph will have edges of type `MaxFlowEdge` defined like this:
    `structure MaxFlowEdge where`
    `  capacity : Nat`
    `  flow : Nat := 0` -/
def findMaxFlow (g : Graph α Nat) (source : Nat) (sink : Nat) : Option (Graph α MaxFlowEdge) :=
  match nullFlowNetwork g with
    | none => none
    | some initialGraph =>
      let preflowGraph : FlowNetwork := initialGraph.initializePreflow source

      let (flowNetwork, head) := preflowGraph.initializeVertexList source sink
      let maxFlowNetwork : FlowNetwork := flowNetwork.relabelToFront head head
      match maxFlowNetwork.updateAllVertexPayloads g.toArray with
        | none => panic! "The array sizes did not match!"
        | some res => some res
      
end Graph
