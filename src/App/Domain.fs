module Go.Domain
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

    let isOnGrid size point = 
            1 <= point.row  
            && point.row <= size 
            && 1 <= point.col 
            && point.col <= size

    let neighbors point size = 
        [
            { point with row = point.row - 1 }
            { point with row = point.row + 1 }
            { point with col = point.col - 1 }
            { point with col = point.col + 1 }
         ] |> Seq.filter(isOnGrid size)

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
        let stones = sg1.stones + sg2.stones
        let stonePoints = stones |> Set.map (fun (Stone p) -> p )
        { color = sg1.color
          stones = stones
          liberties = (sg1.liberties + sg2.liberties) - (stonePoints |> Seq.map (fun pt -> (Liberty pt)) |> Set.ofSeq)
        }

    type Grid = Map<Point, StoneGroup>

    let removePoint (grid : Grid) point= 
        grid.Remove point

    let replaceStoneGroup (grid :Grid) point stoneGroup   =
        grid.Add (point, stoneGroup)

    let replaceWholeStoneGroup (grid : Grid) stoneGroup = 
        stoneGroup.stones
        |> Set.fold (fun gr (Stone pt) -> (replaceStoneGroup gr pt stoneGroup)) grid

    type Board = {
        size: int
        grid: Grid
    }

    let getStoneGroup (grid : Grid) point  = 
        grid.TryFind point
      
    let removeStoneGroup grid stoneGroup size = 
        let updateNeighborStoneGroup nb pt grd =
            let neighborStoneGroup = getStoneGroup grd nb
            match neighborStoneGroup with
            | Some sg -> if sg <> stoneGroup 
                         then 
                             let nbStoneGroup = addLiberty sg pt
                             replaceWholeStoneGroup grd nbStoneGroup
                         else grd
            | None -> grd

        stoneGroup.stones 
        |> Set.fold (fun gr (Stone pt) ->   let neighborsUpdated  p gr = (neighbors p size)
                                                                         |> Seq.fold (fun g nb -> updateNeighborStoneGroup nb p g) gr
                                            removePoint (neighborsUpdated pt gr) pt) grid
 
    type NeighbourState = private { 
                                    adjacentSameColor: Set<StoneGroup>
                                    adjacentOppositeColor: Set<StoneGroup>
                                    liberties: Set<Liberty>
                                  }
    let placeStone board (Player col) point = 
        let neighbourState = (neighbors point board.size)
                             |> Seq.fold (fun state nb ->  let neighborStoneGroup = getStoneGroup board.grid nb
                                                           match neighborStoneGroup with
                                                           | Some sg -> if (sg.color = col) 
                                                                        then {state with adjacentSameColor = state.adjacentSameColor.Add sg}  
                                                                        else { state with adjacentOppositeColor = state.adjacentOppositeColor.Add sg}
                                                           | None -> {state with liberties = state.liberties.Add (Liberty nb) })
                                                           { adjacentSameColor = Set.empty
                                                             adjacentOppositeColor = Set.empty
                                                             liberties = Set.empty}
        let newStoneGroup = neighbourState.adjacentSameColor 
                            |> Set.fold (merge) {   color = col
                                                    stones = Set.singleton (Stone point)
                                                    liberties = neighbourState.liberties}
        let oppositeColorRemovedLiberties = neighbourState.adjacentOppositeColor 
                                            |> Set.map (fun sg-> removeLiberty sg point)
        let newGrid = oppositeColorRemovedLiberties
                      |> Set.fold (fun gr sg -> if Set.count sg.liberties = 0
                                                then removeStoneGroup gr sg board.size
                                                else replaceWholeStoneGroup gr sg) (replaceWholeStoneGroup board.grid newStoneGroup)
        { board with grid = newGrid }

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
                    grid = Map.empty
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

    let doesViolateKo gameState player move =
        let rec equalsPreviousState gameState player nextBoard =
            match gameState with
            | None -> false
            | Some gs -> if gs.nextPlayer = other player && gs.board = nextBoard 
                         then true
                         else equalsPreviousState gs.previousState player nextBoard
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
        | Some _  -> false
        | None ->   let nbs = neighbors point board.size
                    match (nbs |> Seq.tryFind (fun nb -> let nbSg = getStoneGroup board.grid nb
                                                         nbSg.IsSome && nbSg.Value.color <> color)) with
                    | Some _ -> false
                    | None ->   let corners =   let col = point.col
                                                let row = point.row
                                                [
                                                    { row = row - 1; col = col - 1}
                                                    { row = row - 1; col = col + 1 }
                                                    { row = row + 1; col = col - 1 }
                                                    { row = row + 1; col = col + 1 }
                                                ] |> Seq.filter (isOnGrid board.size)
                                let friendlyCorners = corners
                                                    |> Seq.filter (fun corner -> let cornerSg = getStoneGroup board.grid corner
                                                                                 cornerSg.IsSome && cornerSg.Value.color = color
                                                                  )
                                                    |> Seq.length
                                let offBoardCorners = 4 - (Seq.length corners)
                                if offBoardCorners > 0
                                then offBoardCorners + friendlyCorners = 4
                                else friendlyCorners >= 3
                        

