module Go.MonteCarloTreeSearch.Test

open Xunit
open Go.Game
open Go.MonteCarloTreeSearch

[<Fact>]
let ``updates tree correctly`` () =
    let gameState = newGame 3
    let numSimulations = 25
    let node = seq { 1 .. numSimulations} |> Seq.fold (fun state _ -> select state |> snd) (createNode gameState)

    let rec childCount node = 
        if node.children.Count = 0
        then 0 
        else Map.fold (fun state _ v -> state + (childCount v)) node.children.Count node.children

    Assert.Equal(numSimulations, childCount node)
    Assert.Equal(numSimulations, node.numRollouts)
    Assert.Equal(numSimulations, (Map.find (Player White) node.winCounts) + (Map.find (Player Black) node.winCounts))
