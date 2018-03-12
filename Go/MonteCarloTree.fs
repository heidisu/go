module Go.MonteCarloTree
    open Go.Game
    open System

    let utcScore parentRollouts childRollouts winPct = 
        let exploration = Math.Sqrt (Math.Log(float parentRollouts)/(float childRollouts))
        winPct + 1.5 * exploration

    type MCTSNode = { 
        possibleMoves: seq<Point>
        children: Map<Point, MCTSNode>
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

    let merge node1 node2 = 
        let mergedWincounts = 
            [Player Black; Player White]
            |> Seq.map (fun p -> (p, (Map.find p node1.winCounts) + (Map.find p node2.winCounts)))
            |> Seq.fold (fun map pair -> Map.add (fst pair) (snd pair) map) Map.empty
        {
            possibleMoves = node1.possibleMoves
            gameState = node1.gameState
            numRollouts = node1.numRollouts + node2.numRollouts
            winCounts = mergedWincounts
            children = Map.empty
        }

    let unvisitedMoves node = 
        validMoves node.gameState
        |> Seq.filter (fun pt -> not (Map.containsKey pt node.children))

    let canAddChild node = 
        Seq.length (unvisitedMoves node) > 0

    let isTerminal node = 
        isOver node.gameState   

    let winningPercent node player = 
        Map.tryFind player node.winCounts
           |> Option.map (fun i -> (float i) / (float node.numRollouts) ) 
           |> Option.defaultValue 0.0

    let selectChild player node = 
            let totalRollouts = node.children |> Map.fold (fun state _ node -> state + node.numRollouts) 0
            node.children
            |> Map.toList
            |> Seq.map (fun (_,c) -> (c, utcScore totalRollouts c.numRollouts (winningPercent node player)))
            |> Seq.maxBy (fun (_, n) -> n)
            |> fst

    let random = Random()
    let addRandomChild node = 
        let possibleMoves = unvisitedMoves node
        let index = random.Next (Seq.length possibleMoves - 1)
        let newMove = possibleMoves |> Seq.item index
        let newGameState = applyMove node.gameState node.gameState.nextPlayer (Play newMove)
        let child = createNode newGameState
        { node with children = Map.add newMove child node.children}


    let private select node  = 
        if canAddChild node
        then addRandomChild node
        elif isTerminal node
        then node
        else selectChild node.gameState.nextPlayer node

    let rec private updateWinningState (node : Option<MCTSNode>) winner =
        0
        //match node with
        //| Some n -> addWinner winner node
        //            updateWinningState n.Parent winner
        //| None -> ()

    let simulateRandomGame gameState = 
        Player Black

    let selectMove gameState numRounds = 
        let root =
            seq { 1 .. numRounds}
            |> Seq.fold (fun node _ -> merge node (select node)) (createNode gameState)

        root.children
        |> Map.toSeq
        |> Seq.map (fun (move, child) -> (winningPercent child gameState.nextPlayer, move))
        |> Seq.maxBy (fun (p,_) -> p)
        |> (fun (_, pt) -> Play pt)