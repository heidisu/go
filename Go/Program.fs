open Go.Game
open Go.Agents
open System

let cols = "ABCDEFGHIJKLMNOPQRST"
let stoneToChar sg =
    match sg with
    | None -> "."
    | Some sg -> if sg.color = Black then "x" else "o"

let printMove player move = 
    let moveString =
        match move with
        | Pass -> "passes"
        | Resign -> "resigns"
        | Play pt -> sprintf"%c%d" (cols |> Seq.item (pt.col-1)) pt.row
    let playerString = 
        match player with
        | Player Black -> "black"
        | Player White -> "white"
    printfn "%s %s" playerString moveString

let printBoard board =
     for i in board.size .. -1 .. 1 do 
        printf "%d " i
        for j in 1 .. board.size do
            let stoneGroup = getStoneGroup board.grid { row = i
                                                        col = j}
            printf "%s" (stoneToChar stoneGroup)
        printfn ""
     printfn "  %s" (cols |> Seq.take board.size |> String.Concat)


[<EntryPoint>]
let main argv =
    let boardSize = 9
    let mutable game = newGame boardSize
    let bots player = 
        match player with
        | Player Black -> RandomAgent
        | Player White -> RandomAgent

    while not (isOver game) do
        printBoard game.board
        let move = selectMove game (bots game.nextPlayer)
        printMove game.nextPlayer move
        game <- applyMove game game.nextPlayer move 
    printfn "GAME OVER"
    0 // return an integer exit code
