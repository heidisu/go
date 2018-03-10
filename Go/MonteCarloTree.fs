module Go.MonteCarloTree
    open Go.Game
    open System

    let utcScore parentRollouts childRollouts winPct = 
        let exploration = Math.Sqrt (Math.Log(float parentRollouts)/(float childRollouts))
        winPct + 1.5 * exploration

    type private MCTSNode(gameState, parent, move) =
        let mutable winCounts = Map.empty |> Map.add (Player Black) 0 |> Map.add (Player White) 0
        let mutable numRollouts = 0
        let mutable children = Seq.empty
        let mutable unvisitedMoves = Seq.empty
        let random = Random()
        member this.GameState = gameState
        member this.Move = move
        member this.Parent = parent

        member this.Children = children
        member this.NumRollouts = numRollouts

        member this.AddWinner winner = 
            let prevCount = winCounts.TryFind winner |> Option.defaultValue 0
            winCounts <- winCounts |> Map.add winner (prevCount + 1)
            numRollouts <- numRollouts + 1

        member this.AddRandomChild = 
            let index = random.Next (Seq.length unvisitedMoves - 1)
            let newMove = unvisitedMoves |> Seq.item index
            let newGameState = applyMove gameState gameState.nextPlayer newMove
            let child = MCTSNode(newGameState, Some this, Some newMove)
            children <- Seq.append [child] children
            child

        member this.CanAddChild = 
            Seq.length unvisitedMoves > 0

        member this.IsTerminal = 
            isOver gameState   

        member this.WinningPercent player = 
            Map.tryFind player winCounts
            |> Option.map (fun i -> (float i) / (float numRollouts) ) 
            |> Option.defaultValue 0.0

        member this.SelectChild player = 
            let totalRollouts = Seq.sumBy (fun (c : MCTSNode) -> c.NumRollouts) children
            children
            |> Seq.map (fun c -> (c, utcScore totalRollouts c.NumRollouts (c.WinningPercent player)))
            |> Seq.maxBy (fun (_, n) -> n)
            |> fst



    let private selectNode (node :MCTSNode) player = 
        if node.CanAddChild
        then node.AddRandomChild
        elif node.IsTerminal
        then node
        else node.SelectChild player

    let rec private updateWinningState (node : Option<MCTSNode>) winner =
        match node with
        | Some n -> n.AddWinner winner
                    updateWinningState n.Parent winner
        | None -> ()

    let simulateRandomGame gameState = 
        Player Black

    let selectMove gameState numRounds = 
        let root = MCTSNode(gameState, None, None)
        for i in 1 .. numRounds do
            let node = selectNode root gameState.nextPlayer
            let winner = simulateRandomGame node
            updateWinningState (Some node) winner

        root.Children
        |> Seq.map (fun c -> (c.WinningPercent gameState.nextPlayer, c.Move))
        |> Seq.maxBy (fun (p, m) -> p)
        |> snd