open Go.Game
open Go.Agents
open Go.Scoring
open System

let cols = "ABCDEFGHIJKLMNOPQRST"
let stoneToChar sg =
    match sg with
    | None -> "."
    | Some sg -> if sg.color = Black then "x" else "o"

let printPlayer player = 
    match player with
    | Player Black -> "black"
    | Player White -> "white"

let printAgent agent = 
    match agent with
    | RandomAgent -> "random"
    | MonteCarloAgent -> "monte carlo"

let printMove player move = 
    let moveString =
        match move with
        | Pass -> "passes"
        | Resign -> "resigns"
        | Play pt -> sprintf"%c%d" (cols |> Seq.item (pt.col-1)) pt.row
    printfn "%s %s" (printPlayer player) moveString

let printBoard board =
     for i in board.size .. -1 .. 1 do 
        printf "%d " i
        for j in 1 .. board.size do
            let stoneGroup = getStoneGroup board.grid { row = i
                                                        col = j}
            printf "%s" (stoneToChar stoneGroup)
        printfn ""
     printfn "  %s" (cols |> Seq.take board.size |> String.Concat)

let bots player = 
        match player with
        | Player Black -> RandomAgent
        | Player White -> MonteCarloAgent

let rec play bots game = 
    match (isOver game) with
    | true ->  let gameResult = getGameResult game
               printfn "GAME OVER"
               printfn "Winner: %s" (printPlayer gameResult.winner)
               printfn "Winning margin: %f" gameResult.winningMargin
    | false -> printBoard game.board
               let move = selectMove game (bots game.nextPlayer)
               printMove game.nextPlayer move
               play bots (applyMove game game.nextPlayer move)

let playGame boardSize = newGame boardSize |> play bots
let benchmark boardSize numPlays = 
    let bots (agent1, agent2) = 
        fun player ->
            match player with   
            | Player White -> agent1
            | Player Black -> agent2
    let rec play bots game = 
        match isOver game with
        | true -> getGameResult game
        | false -> let move = selectMove game (bots game.nextPlayer)
                   play bots (applyMove game game.nextPlayer move)
    let printSimulation bots gameResults = 
        let winGroups =  gameResults
                         |> Seq.groupBy (fun gameRes -> gameRes.winner)
                         |> Seq.map (fun (pl, res) -> (pl, Seq.length res))
        let winCounts player =  
            winGroups
            |> Seq.tryFind (fun (pl, _) -> pl = player )
            |> Option.map snd
            |> Option.defaultValue 0
        printfn "White player: %s black player: %s white wins: %i black wins: %i" 
            (printAgent (Player White |> bots))
            (printAgent (Player Black |> bots))
            (Player White |> winCounts)
            (Player Black |> winCounts)
    let agents = [RandomAgent; MonteCarloAgent]
    agents
    |> List.collect (fun ag1 -> agents |> List.map (fun ag2 -> (ag1, ag2)))
    |> List.map ( bots)
    |> List.iter (fun bots ->  let results = seq{ 1 .. numPlays} 
                                             |> Seq.fold (fun state _ -> Seq.append state (Seq.singleton (play bots (newGame boardSize)))) Seq.empty
                               printSimulation bots results)                                       
                                           
[<EntryPoint>]
let main argv  =
    let boardSize = 5
    playGame boardSize
    //benchmark boardSize 100
    0 
