module Go.RandomPlay

open Go.Game

 let selectRandomMove gameState =
    let rnd = System.Random()
    let candidates =  validMoves gameState

    if Seq.isEmpty candidates
    then Pass
    else
        let randomIndex = rnd.Next (Seq.length candidates - 1)
        Seq.item randomIndex candidates 