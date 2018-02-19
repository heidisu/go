module Go.Agents
    open Go.Game
    
    type Agent = RandomAgent

    let private selectRandomMove gameState =
        let rnd = System.Random()
        let candidates = 
            seq { for i in 1 .. gameState.board.size 
                    do for j in 1 .. gameState.board.size do yield (i, j)}
            |> Seq.filter (fun (x, y) ->    let pt = {
                                                        row = x
                                                        col = y
                                                     }
                                            isValidMove gameState (Play pt) &&
                                            not (pointIsEye gameState.board pt gameState.nextPlayer)
            )

        if Seq.isEmpty candidates
        then Pass
        else
            let randomIndex = rnd.Next (Seq.length candidates - 1)
            let random = Seq.item randomIndex candidates 
            Play {
                    row = (fst random)
                    col = (snd random)
                 }

    let selectMove gameState agent = 
        match agent with
        | RandomAgent -> selectRandomMove gameState
    