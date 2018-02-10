module Domain
    open System

    type Color = Black | White

    type Player = Player of Color

    let other player =
        match player with
        | Player Black -> Player White
        | Player White -> Player Black

    type Point = {
        row: int
        col: int
    }

    let neighbors point size = 
        let isOnGrid point = 
            1 <= point.row  
            && point.row <= size 
            && 1 <= point.col 
            && point.col <= size
        [
            { point with row = point.row - 1 }
            { point with row = point.row + 1 }
            { point with row = point.col - 1 }
            { point with row = point.col + 1 }
        ] |> Seq.filter isOnGrid

    type Move = 
    | Play of Point
    | Pass
    | Resign    

    type Stone = Stone of Point

    type Liberty = Liberty of Point

    type StoneGroup = {
        color: Color
        stones : Set<Stone>
        liberties: Set<Liberty>
    }

    let removeLiberty stoneGroup point = 
        { stoneGroup 
            with liberties = stoneGroup.liberties 
                             |> Set.filter (fun (Liberty p) -> not ( p = point))}
    
    
    let addLiberty stoneGroup point = 
        { stoneGroup 
          with liberties = stoneGroup.liberties 
                           |> Set.add (Liberty point)}

    let merge sg1 sg2 =
        let stones = Set.union sg1.stones sg2.stones
        let stonePoints = stones |> Set.map (fun (Stone p) -> p )
        { color = sg1.color
          stones = stones
          liberties = (Set.union sg1.liberties sg2.liberties
                       |> Set.filter (fun ( Liberty p) -> not (stonePoints |> Set.contains p))) }

    type Grid = seq<Point * StoneGroup>

    let removePoint grid point = 
        grid |> Seq.filter (fun (p, _) -> p <> point)

    let replaceStoneGroup grid point stoneGroup =
        let newGrid = removePoint grid point
        newGrid |> Seq.append (Seq.singleton (point, stoneGroup))

    type Board = {
        size: int
        grid: Grid
    }

    let getStoneGroup grid point = 
        grid
        |> Seq.tryFind (fun (p, _) -> p = point) 
        |> Option.map (fun (_, sg) -> sg)
    
    let removeStoneGroup grid stoneGroup size = 
        let mutable newGrid = grid
        let updateNeighborStoneGroup nb =
            let neighborStoneGroup = getStoneGroup grid nb
            match neighborStoneGroup with
            | Some sg -> if sg <> stoneGroup 
                         then 
                             let nbStoneGroup = addLiberty sg nb
                             newGrid <- replaceStoneGroup grid nb nbStoneGroup
            | None -> ()

        for (Stone p ) in stoneGroup.stones do
            for neighbor in (neighbors p size) do
                updateNeighborStoneGroup neighbor
            newGrid <- (removePoint grid p)
        newGrid

    // assumes ((isOnGrid board point) && board.grid.ContainsKey point) 
    let placeStone board (Player col) point = 
        let mutable adjacentSameColor = Set.empty
        let mutable  adjacentOppositeColor = Set.empty
        let mutable liberties = Set.empty
        let mutable grid = board.grid
        for nb in (neighbors point board.size) do
            let neighborStoneGroup = getStoneGroup grid nb
            match neighborStoneGroup with
            | Some sg -> 
                if (sg.color = col) 
                then adjacentSameColor <- (adjacentSameColor.Add sg) 
                else adjacentOppositeColor <- (adjacentOppositeColor.Add sg)
            | None -> liberties <- (liberties |> Set.add (Liberty nb))
        let mutable newStoneGroup = { 
                                color = col
                                stones = Set.singleton (Stone point)
                                liberties = liberties
                                }
        for sameColorSg in adjacentSameColor do
            newStoneGroup <- merge newStoneGroup sameColorSg
        for (Stone p) in newStoneGroup.stones do
            grid <- replaceStoneGroup grid p newStoneGroup
        let oppositeColorRemovedLiberties = adjacentOppositeColor 
                                            |> Set.map (fun sg -> removeLiberty sg point)
        for sg in oppositeColorRemovedLiberties do
            if Set.count sg.liberties = 0
            then grid <- removeStoneGroup grid sg board.size
        { board with grid = grid }

    type GameState = {
        board: Board
        nextPlayer : Player
        previousState : Option<GameState>
        lastMove : Option<Move>
    }

    let applyMove gameState player move = 
        if player <> gameState.nextPlayer then raise (Exception "fail")
        match move with
        | Play p -> let nextBoard = placeStone gameState.board player p
                    { 
                        board = nextBoard 
                        nextPlayer = other player
                        previousState = Some gameState
                        lastMove = Some move
                    }
        | _ -> { 
                    board = gameState.board
                    nextPlayer = other player
                    previousState = Some gameState
                    lastMove = Some move
                }

    let newGame boardSize = {
        board = { 
                    size = boardSize 
                    grid = Seq.empty
                }
        nextPlayer = Player Black
        previousState = None
        lastMove = None
    }         

    let isOver gameState = 
        match gameState.lastMove with
        | None -> false
        | Some mv -> let secondLast = 
                        match gameState.previousState with 
                        | None -> None
                        | Some gs -> gs.lastMove
                     if secondLast = None then false
                     else mv = Pass && secondLast = Some Pass

    let isMoveSelfCapture gameState player move =
        match move with
        | Play pt -> let nextBoard = placeStone gameState.board player pt
                     let nextStoneGroup = getStoneGroup nextBoard.grid pt
                     match nextStoneGroup with
                     | None -> false
                     | Some sg -> sg.liberties.Count = 0
        | _ -> false

    let rec equalsPreviousState gameState player nextBoard =
        match gameState with
        | None -> false
        | Some gs -> if gs.nextPlayer = other player && gs.board = nextBoard 
                     then true
                     else equalsPreviousState gs.previousState player nextBoard

    let doesViolateKo gameState player move =
        match move with
        | Play pt -> let nextBoard = placeStone gameState.board player pt
                     let pastState = gameState.previousState
                     equalsPreviousState pastState player nextBoard
        | _ -> false


    let isValidMove gameState move = 
        if isOver gameState then false
        else match move with
                | Play pt -> Option.isNone (getStoneGroup gameState.board.grid pt) &&
                             not (isMoveSelfCapture gameState gameState.nextPlayer move) &&
                             not (doesViolateKo gameState gameState.nextPlayer move)
                | _ -> true

    let pointIsEye board point (Player color) =
        match getStoneGroup board.grid point with   
        | None -> false
        | Some _ -> let nbs = neighbors point board.size
                    let unfriendlyCorners = nbs
                                            |> Seq.filter (fun nb -> let nbSg = getStoneGroup board.grid nb
                                                                     nbSg.IsSome && nbSg.Value.color <> color
                                             )
                                             |> Seq.length
                    if unfriendlyCorners > 0
                    then false 
                    else
                        let offBoardCorners = 4 - (Seq.length nbs)
                        let friendlyCorners = 4 - offBoardCorners - unfriendlyCorners
                        if offBoardCorners > 0 
                        then offBoardCorners + friendlyCorners = 4
                        else friendlyCorners >= 3
                        
    
    let selectMove gameState =
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

