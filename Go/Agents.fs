module Go.Agents
    open Go.Game
    
    type Agent = RandomAgent

    let validMoves gameState = 
        seq { for i in 1 .. gameState.board.size 
                    do for j in 1 .. gameState.board.size do yield {row = i; col = j}}
        |> Seq.filter (fun pt ->  isValidMove gameState (Play pt) &&
                                  not (pointIsEye gameState.board pt gameState.nextPlayer))

    let private selectRandomMove gameState =
        let rnd = System.Random()
        let candidates =  validMoves gameState

        if Seq.isEmpty candidates
        then Pass
        else
            let randomIndex = rnd.Next (Seq.length candidates - 1)
            let randomPt = Seq.item randomIndex candidates 
            Play randomPt

    let selectMove gameState agent = 
        match agent with
        | RandomAgent -> selectRandomMove gameState
    