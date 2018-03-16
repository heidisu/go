module Go.MonteCarloTree.Test

open Xunit
open Go.Game
open Go.MonteCarloTree

[<Fact>]
let ``updates tree correctly`` () =
    let gameState = newGame 3
    let node = select (createNode gameState)
    Assert.Equal (1,  node.children.Count)
    let child =  Map.fold (fun state _ v -> Seq.append state  (Seq.singleton v) ) Seq.empty node.children |> Seq.item 0
    Assert.Equal(Map.find (Player Black) child.winCounts, Map.find (Player Black) node.winCounts)
    Assert.Equal(Map.find (Player White) child.winCounts, Map.find (Player White) node.winCounts)

    let node2 = select node
    Assert.Equal(2, node2.children.Count)

    let node12 = seq { 1 .. 10} |> Seq.fold (fun state _ -> select state) node2 

    let rec childCount node = 
        if node.children.Count = 0
        then 0 
        else Map.fold (fun state k v -> state + (childCount v)) node.children.Count node.children
    Assert.Equal(12, childCount node12)
