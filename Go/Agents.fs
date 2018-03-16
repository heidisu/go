module Go.Agents
    open Go.Game
    
    type Agent = RandomAgent

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
    