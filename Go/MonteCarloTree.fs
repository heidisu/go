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
    
    let createNodeFromWinner gameState winner =
        let node = createNode gameState
        { node with numRollouts = 1; winCounts = Map.add winner 1 node.winCounts }

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
            |> Seq.map (fun (mv,c) -> ((mv, c), utcScore totalRollouts c.numRollouts (winningPercent node player)))
            |> Seq.maxBy (fun (_, n) -> n)
            |> fst

    let random = Random()
    let addRandomChild node = 
        let possibleMoves = unvisitedMoves node
        let index = random.Next (Seq.length possibleMoves - 1)
        let newMove = possibleMoves |> Seq.item index
        let newGameState = applyMove node.gameState node.gameState.nextPlayer (Play newMove)
        (newMove, createNode newGameState)

    let getRandomMove node = 
        let possibleMoves = unvisitedMoves node
        let index = random.Next (Seq.length possibleMoves - 1)
        possibleMoves |> Seq.item index

    let simulateRandomGame gameState = 
        Player Black
    
    let rec private updateWinningState node winner =
        let prevCount = Map.find winner node.winCounts
        {node with winCounts = Map.add winner (prevCount + 1) node.winCounts; numRollouts = node.numRollouts + 1}
        //match node with
        //| Some n -> addWinner winner node
        //            updateWinningState n.Parent winner
        //| None -> ()
    
    //let backprop node child winner = 
    //    { node with children =  Map.add move updatedChild node.childre }

    let rec select node  = 
        if not (canAddChild node) && not (isTerminal node)
        then let (move, child) = selectChild node.gameState.nextPlayer node
             let expanded = select child
             { node with children = Map.add move expanded node.children}
        elif canAddChild node
        then let move = getRandomMove node
             let nextState = applyMove node.gameState node.gameState.nextPlayer (Play move)
             let winner = simulateRandomGame nextState
             let child =  createNodeFromWinner nextState winner
             { node with children = Map.add move child node.children; numRollouts = node.numRollouts + 1; winCounts = Map.add winner ((Map.find winner node.winCounts) + 1) node.winCounts} 
        else node
        

        //f canAddChild node
        //then addRandomChild node
        //elif isTerminal node
        //then node
        //else let exploredNode = selectChild node.gameState.nextPlayer node |> select
          //   updateWinningState node exploredNode

    let selectMove gameState numRounds = 
        let root =
            seq { 1 .. numRounds}
            |> Seq.fold (fun node _ -> select node) (createNode gameState)

        root.children
        |> Map.toSeq
        |> Seq.map (fun (move, child) -> (winningPercent child gameState.nextPlayer, move))
        |> Seq.maxBy (fun (p,_) -> p)
        |> (fun (_, pt) -> Play pt)