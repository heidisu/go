﻿// Learn more about F# at http://fsharp.org
open Domain
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

    while not (isOver game) do
        printBoard game.board
        let move = Domain.selectMove game
        printMove game.nextPlayer move
        game <- Domain.applyMove game game.nextPlayer move 
    printfn "Hello World from F#!"
    0 // return an integer exit code
