module Go.Agents

open Go.Game
open Go.MonteCarloTreeSearch
open Go.RandomPlay

type Agent = {
    name: string
    selectMove: GameState -> Move
}

let randomAgent = {
    name = "random" 
    selectMove = selectRandomMove
}
let monteCarloAgent = {
    name = "monte carlo"
    selectMove = selectMove 100
}
