module Go.MonteCarloTreeSearch
    open Go.Game
    open Go.Scoring
    open Go.RandomPlay
    open System

    let utcScore parentRollouts childRollouts winPct = 
        let exploration = Math.Sqrt (Math.Log(float parentRollouts)/(float childRollouts))
        winPct + 1.5 * exploration

    type MCTSNode = { 
        possibleMoves: seq<Move>
        children: Map<Move, MCTSNode>
        numRollouts: int
        gameState: GameState
        winCounts: Map<Player, int>
    }

    let createNode gameState = 
        { 
            possibleMoves = (validMoves gameState) 
            children = Map.empty
            numRollouts = 0
            gameState = gameState
            winCounts = Map.empty |>  Map.add (Player Black) 0 |> Map.add (Player White) 0
        }
    
    let createNodeFromWinner gameState winner =
        let node = createNode gameState
        { node with numRollouts = 1; winCounts = Map.add winner 1 node.winCounts }

    let unvisitedMoves node = 
        validMoves node.gameState
        |> Seq.filter (fun mv -> not (Map.containsKey mv node.children))

    let canAddChild node = 
        Seq.length (unvisitedMoves node) > 0

    let isTerminal node = 
        validMoves node.gameState |> Seq.isEmpty

    let winningPercent node player = 
        Map.tryFind player node.winCounts
           |> Option.map (fun i -> (float i) / (float node.numRollouts) ) 
           |> Option.defaultValue 0.0

    let selectChild player node = 
            let totalRollouts = node.children |> Map.fold (fun state _ node -> state + node.numRollouts) 0
            node.children
            |> Map.toList
            |> Seq.map (fun (mv,c) -> ((mv, c), utcScore totalRollouts c.numRollouts (winningPercent node player)))
            |> Seq.maxBy (fun (_, n) -> n)
            |> fst

    let random = Random()
    let addRandomChild node = 
        let possibleMoves = unvisitedMoves node
        let index = random.Next (Seq.length possibleMoves - 1)
        let newMove = possibleMoves |> Seq.item index
        let newGameState = applyMove node.gameState node.gameState.nextPlayer newMove
        (newMove, createNode newGameState)

    let getRandomMove node = 
        let possibleMoves = unvisitedMoves node
        let index = random.Next (Seq.length possibleMoves - 1)
        possibleMoves |> Seq.item index

    let simulateRandomGame gameState = 
        let rec play game = 
            match (isOver game) with
            | true ->  let gameResult = getGameResult game
                       gameResult.winner
            | false -> let move = selectRandomMove game
                       play (applyMove game game.nextPlayer move)
        play gameState

    
    let rec private updateWinningState node child move winner =
        let prevCount = Map.find winner node.winCounts
        { node with children = Map.add move child node.children
                    numRollouts = node.numRollouts + 1
                    winCounts = Map.add winner (prevCount + 1) node.winCounts}

    let rec select node  = 
        if not (canAddChild node) && not (isTerminal node)
        then let (move, child) = selectChild node.gameState.nextPlayer node
             let (winner, expanded) = select child
             (winner, updateWinningState node expanded move winner)
        elif canAddChild node
        then let move = getRandomMove node
             let nextState = applyMove node.gameState node.gameState.nextPlayer move
             let winner = simulateRandomGame nextState
             let child =  createNodeFromWinner nextState winner
             (winner, updateWinningState node child move winner)
        else let gameResult = getGameResult node.gameState
             (gameResult.winner, node)

    let selectMove gameState numRounds = 
        if validMoves gameState |> Seq.isEmpty
        then Pass
        else 
            let root =
                seq { 1 .. numRounds}
                |> Seq.fold (fun node _ -> select node |> snd) (createNode gameState)

            root.children
            |> Map.toSeq
            |> Seq.map (fun (move, child) -> (winningPercent child gameState.nextPlayer, move))
            |> Seq.maxBy (fun (p,_) -> p)
            |> (fun (_, move) -> move)