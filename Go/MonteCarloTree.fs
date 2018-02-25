module Go.MonteCarloTree
    open Go.Game
    open System

    type WinConts = Map<Player, int>

    type MCTSNode = { 
                        gameState : GameState
                        winCounts : WinConts
                        numRollouts : int
                        unvisitedMoves: seq<Move>
                        children: seq<MCTSNode>
                    }

    let rnd = Random()
    
    let createNode gameState = 
        let initialWinCounts = (Map.empty.Add(Player Black, 0)).Add (Player White, 0)
        { 
            gameState = gameState
            winCounts = initialWinCounts
            numRollouts = 0
            unvisitedMoves = validMoves gameState |> Seq.map (fun pt -> Play pt)
            children = Seq.empty
        }
    
    let addRandomChild node = 
        let index = rnd.Next (Seq.length node.unvisitedMoves - 1)
        let newMove = node.unvisitedMoves |> Seq.item index
        let newGameState = applyMove node.gameState node.gameState.nextPlayer newMove
        let newChild = createNode newGameState        
        { node with  unvisitedMoves = node.unvisitedMoves |> Seq.filter (fun mv -> mv <> newMove)
                     children = node.children |> Seq.append [newChild] }
    
    let recordWin node winner = 
        let count = match (node.winCounts |> Map.tryFind winner) with
                    | Some x -> x
                    | None -> 0
        { node with winCounts = node.winCounts.Add (winner, count + 1); numRollouts = node.numRollouts + 1 }
    
    let canAddChild node = 
        Seq.length node.unvisitedMoves > 0
    
    let isTerminal node = 
        isOver node.gameState

    let winningPct node player = 
        (Map.tryFind node.winCounts player) 
        |> Option.map (fun i -> (float i) / (float node.numRollouts) ) 
        |> Option.defaultWith ( fun() -> 0.0)

    let selectChild node = node // todo fix
    
    let rec selectNode node = 
        if canAddChild node || isTerminal node
        then node
        else (selectChild node) |> selectNode

    let selectMove gameState =
        let root = createNode gameState  

    type MCTSTree = 
       | Branch of MCTSNode * MCTSTree list
       | Leaf of MCTSNode