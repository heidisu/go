module Domain

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

    let neighbors point = 
        [
            { point with row = point.row - 1 }
            { point with row = point.row + 1 }
            { point with row = point.col - 1 }
            { point with row = point.col + 1 }
        ]

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
        
    let isOnGrid board point = 
        1 <= point.row  
        && point.row <= board.size 
        && 1 <= point.col 
        && point.col <= board.size
    
    let removeStoneGroup grid stoneGroup = 
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
            for neighbor in (neighbors p) do
                updateNeighborStoneGroup neighbor
            newGrid <- (removePoint grid p)
        newGrid

    // assumes ((isOnGrid board point) && board.grid.ContainsKey point) 
    let placeStone board (Player col) point = 
        let mutable adjacentSameColor = Set.empty
        let mutable  adjacentOppositeColor = Set.empty
        let mutable liberties = Set.empty
        let mutable grid = board.grid
        for nb in (neighbors point) do
            if (isOnGrid board point)
            then 
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
            then grid <- removeStoneGroup grid sg
        { board with grid = grid }




