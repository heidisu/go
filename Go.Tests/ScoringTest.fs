module Go.Scoring.Test

open Xunit
open Go.Game

[<Fact>]
let ``computes correct scoring`` () =
    // .w.ww
    // wwww.
    // bbbww
    // .bbbb
    // .b.b.
    let board = {size = 5; grid =  Map.empty}
    let play col (x, y) board = placeStone board (Player col) {row = x; col = y}
    let filledBoard = 
        board 
        |> play Black (1, 2)
        |> play Black (1, 4)
        |> play Black (2, 2)
        |> play Black (2, 3)
        |> play Black (2, 4)
        |> play Black (2, 5)
        |> play Black (3, 1)
        |> play Black (3, 2)
        |> play Black (3, 3)
        |> play White (3, 4)
        |> play White (3, 5)
        |> play White (4, 1)
        |> play White (4, 2)
        |> play White (4, 3)
        |> play White (4, 4)
        |> play White (5, 2)
        |> play White (5, 4)
        |> play White (5, 5)

    let initialConts = Map.empty 
                           |> Map.add BlackStone 0 
                           |> Map.add BlackTerritory 0 
                           |> Map.add WhiteStone 0 
                           |> Map.add WhiteTerritory 0 
                           |> Map.add Dame 0
    let territoryCount =
        evaluateTerritory filledBoard
        |> Map.fold (fun state _ status -> Map.add status ((Map.find status state) + 1) state ) initialConts

    Assert.Equal (9, Map.find BlackStone territoryCount)
    Assert.Equal (4, Map.find BlackTerritory territoryCount)
    Assert.Equal (9, Map.find WhiteStone territoryCount)
    Assert.Equal (3, Map.find WhiteTerritory territoryCount)
    Assert.Equal (0, Map.find Dame territoryCount)

    let gameResult = getGameResult { board = filledBoard; nextPlayer = (Player Black); lastMove = Some Pass;  previousState = None }

    Assert.Equal (Player White, gameResult.winner)