module Domain
open System.Collections.Generic

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


    type Board = {
        size: int
        grid: Dictionary<Point, StoneGroup>
    }

    let getStoneGroup board point = 
        if board.grid.ContainsKey point
        then Some (board.grid.Item point)
        else None
        
    let isOnGrid board point = 
        1 <= point.row  
        && point.row <= board.size 
        && 1 <= point.col 
        && point.col <= board.size

    // assumes ((isOnGrid board point) && board.grid.ContainsKey point) 
    let placeStone board player point = 
        let mutable adjacent_same_color = Set.empty
        let mutable  adjacent_opposite_color = Set.empty
        let mutable liberties = Set.empty
        let Player c = player
        (neighbors point)
        |> Seq.forall (fun p -> 
            if (isOnGrid board point)
            then 
                let neighborStoneGroup = getStoneGroup board p
                match neighborStoneGroup with
                | Some sg -> 
                    if (sg.color = c) 
                    then adjacent_same_color <- (adjacent_same_color.Add sg) 
                    else adjacent_opposite_color <- (adjacent_opposite_color.Add sg)
                | None -> liberties <- Set.empty
            )
        let newStoneGroup = StoneGroup { 
                                color = c 
                                stones = Set.singleton (Stone point)
                                liberties = liberties
                                }



