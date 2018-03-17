module Go.Agents
    open Go.MonteCarloTreeSearch
    open Go.RandomPlay
    
    type Agent = RandomAgent | MonteCarloAgent

    let selectMove gameState agent = 
        match agent with
        | RandomAgent -> selectRandomMove gameState
        | MonteCarloAgent -> selectMove gameState 100