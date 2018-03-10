module Go.Scoring
    open Go.Game

    type TerritoryStatus = | BlackStone | WhiteStone | BlackTerritory | WhiteTerritory | Dame
    type GameResult = {winner: Player; winningMargin: float}

    let rec collectRegion point board (visited : Set<Point>) = 
        if visited.Contains point
        then (Set.empty, Set.empty)
        else
            let stoneGroupColor = (getStoneGroup board.grid point) |> Option.map (fun sg -> sg.color)
            Set.ofList [(-1, 0); (1, 0); (0, -1); (0, 1)]
            |> Set.map (fun (x, y) -> {row = point.row + x; col = point.col + y})
            |> Set.filter (isOnGrid board.size)
            |> Set.fold (fun  state pt -> 
                                           let nbGroupColor = (getStoneGroup board.grid pt) |> Option.map (fun sg -> sg.color)
                                           match (stoneGroupColor, nbGroupColor) with
                                           | (Some c1, Some c2) when c1 = c2 -> let nbRegion = collectRegion pt board (visited + (Set.singleton point))
                                                                                ((fst state) + (fst nbRegion), (snd state) - (snd nbRegion))
                                           | (None, Some c) -> (fst state, (snd state) + (Set.singleton c ))
                                           | _ -> state)
                        (Set.singleton point, Set.empty)

    let evaluateTerritoryForPoint board point (map : Map<Point, TerritoryStatus>) = 
        if map.ContainsKey point
        then map
        else let stoneGroup = getStoneGroup board.grid point
             match stoneGroup with
             | None ->  let (group, neighbors) = collectRegion point board Set.empty
                        let territoryStatus = 
                            match (Set.count neighbors) with
                            | 1 ->  let color = neighbors |> Set.toSeq |> Seq.item 0
                                    match color with
                                    | Color.Black -> BlackTerritory
                                    | Color.White -> WhiteTerritory
                            | _ -> Dame
                        group |> Set.fold (fun map pt -> Map.add pt territoryStatus map) map 
             | Some sg -> let status = match sg.color with
                                       | Color.Black -> BlackStone
                                       | Color.White -> WhiteStone
                          Map.add point status map

    let evaluateTerritory board = 
         seq { for i in 1 .. board.size 
                    do for j in 1 .. board.size do yield {row = i; col = j}}
         |> Seq.fold (fun map pt -> evaluateTerritoryForPoint board pt map) Map.empty

    let getGameResult gameState =
        let komi = 7.5
        let initialConts = Map.empty 
                           |> Map.add BlackStone 0 
                           |> Map.add BlackTerritory 0 
                           |> Map.add WhiteStone 0 
                           |> Map.add WhiteTerritory 0 
                           |> Map.add Dame 0
        let territoryCount = 
            evaluateTerritory gameState.board
            |> Map.fold (fun state _ status -> Map.add status ((Map.find status state) + 1) state ) initialConts
        let blackSum = (Map.find BlackStone territoryCount) + (Map.find BlackTerritory territoryCount) |> float
        let whiteSum = (Map.find WhiteStone territoryCount) + (Map.find WhiteTerritory territoryCount) |> float
        let winner = if blackSum > whiteSum + komi then Player Black else Player White
        let margin = abs (blackSum - whiteSum - komi)
        { winner = winner; winningMargin = margin}