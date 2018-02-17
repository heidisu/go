module Tests

open System
open Xunit
open Go.Domain

[<Fact>]
let ``inner point has four neighbors`` () =
    let neighbors = neighbors { row = 2; col = 3} 5
    let expectNeighbors = Set.ofList [{row = 1; col = 3}; {row = 2; col = 2}; {row = 2; col = 4}; {row = 3; col = 3}]
    Assert.Equal(4, Seq.length neighbors)
    Assert.Equal<Set<Point>>(expectNeighbors, Set.ofSeq neighbors)


[<Fact>]
let ``edge point has three neighbors`` () =
    let neighbors = neighbors { row = 4; col = 1} 5
    let expectNeighbors = Set.ofList [{row = 4; col = 2}; {row = 3; col = 1}; {row = 5; col = 1}]
    Assert.Equal(3, Seq.length neighbors)
    Assert.Equal<Set<Point>>(expectNeighbors, Set.ofSeq neighbors)

[<Fact>]
let ``corner point has two neighbors`` () =
    let neighbors = neighbors { row = 5; col = 5} 5
    let expectNeighbors = Set.ofList [{row = 5; col = 4}; {row = 4; col = 5}]
    Assert.Equal(2, Seq.length neighbors)
    Assert.Equal<Set<Point>>(expectNeighbors, Set.ofSeq neighbors)

[<Fact>]
let ``Add new liberty adds new liberty`` () = 
    let stonegroup = {
                        color = White
                        stones = Set.singleton (Stone { row = 4; col = 1})
                        liberties = Set.ofList [Liberty {row = 4; col = 2}; Liberty {row = 3; col = 1}] 
                     }
    let point = {row = 5; col = 1}
    let newStoneGroup = addLiberty stonegroup point
    Assert.Equal<Set<Liberty>>(stonegroup.liberties + Set.singleton (Liberty point), newStoneGroup.liberties)

[<Fact>]
let ``Add existing liberty does not change liberties`` () = 
    let stonegroup = {
                        color = White
                        stones = Set.singleton (Stone { row = 4; col = 1})
                        liberties = Set.ofList [Liberty {row = 4; col = 2}; Liberty {row = 3; col = 1}] 
                     }
    let newStoneGroup = addLiberty stonegroup {row = 4; col = 2}
    Assert.Equal<Set<Liberty>>(stonegroup.liberties, newStoneGroup.liberties)

[<Fact>]
let ``Remove existing liberty removes liberty`` () = 
    let stonegroup = {
                        color = White
                        stones = Set.singleton (Stone { row = 4; col = 1})
                        liberties = Set.ofList [Liberty {row = 4; col = 2}; Liberty {row = 3; col = 1}] 
                     }
    let point = {row = 4; col = 2}
    let newStoneGroup = removeLiberty stonegroup point
    Assert.Equal<Set<Liberty>>(stonegroup.liberties - Set.singleton (Liberty point), newStoneGroup.liberties)

[<Fact>]
let ``Remove non existing liberty does not change liberties`` () = 
    let stonegroup = {
                        color = White
                        stones = Set.singleton (Stone { row = 4; col = 1})
                        liberties = Set.ofList [Liberty {row = 4; col = 2}; Liberty {row = 3; col = 1}] 
                     }
    let point = {row = 5; col = 1}
    let newStoneGroup = removeLiberty stonegroup point
    Assert.Equal<Set<Liberty>>(stonegroup.liberties, newStoneGroup.liberties)

[<Fact>]
let ``Stonegroups are merged correctly`` () = 
    let sg1 = {
                color = Black
                stones = Set.ofList [Stone {row = 3; col = 3}; Stone {row = 4; col = 3}]
                liberties = Set.ofList [ 
                                        Liberty {row = 2; col = 3}; 
                                        Liberty {row = 3; col = 2}; 
                                        Liberty {row = 3; col = 4}; 
                                        Liberty {row = 4; col = 2}; 
                                        Liberty {row = 4; col = 4}; 
                                        Liberty {row = 5; col = 3}]

              }
    let point = {row = 3; col = 2}
    let sg2 = {
                color = Black
                stones = Set.singleton (Stone point)
                liberties = (neighbors point 5) |> Seq.map (fun pt -> Liberty pt) |> Set.ofSeq
              }
    let expectedMerge =  {
                            color = Black
                            stones = Set.add (Stone point) sg1.stones
                            liberties = Set.ofList [
                                            Liberty {row = 2; col = 3}
                                            Liberty {row = 3; col = 4} 
                                            Liberty {row = 4; col = 2} 
                                            Liberty {row = 4; col = 4} 
                                            Liberty {row = 5; col = 3}
                                            Liberty {row = 3; col = 1}
                                            Liberty {row = 2; col = 2}
                            ]
                          }
    Assert.Equal<StoneGroup>(expectedMerge, merge sg1 sg2)

[<Fact>]
let ``grid is updated correctly when stone is placed`` () = 
    let board = { size = 5; grid = Seq.empty }
    let blackPt = { row = 3; col = 3 }
    let whitePt = {row = 3; col = 2}
    let firstUpdateBoard = placeStone board (Player Black) blackPt
    let secondUpdate = placeStone firstUpdateBoard (Player White) whitePt
    let expectedGrid = [
                         (whitePt, {color = White; stones = Set.singleton (Stone whitePt); liberties = Set.ofList [Liberty {row = 2; col = 2}; Liberty { row = 3; col = 1}; Liberty { row = 4; col = 2}]}); 
                         (blackPt, {color = Black; stones = Set.singleton (Stone blackPt); liberties = Set.ofList [Liberty {row = 2; col = 3}; Liberty { row = 3; col = 4}; Liberty { row = 4; col = 3}]})
                       ]
    Assert.Equal<Set<Point * StoneGroup>>(Set.ofSeq expectedGrid, Set.ofSeq secondUpdate.grid)


[<Fact>]
let ``stonegroup is removed correctly from grid`` () =
    let board = { size = 5; grid = Seq.empty }
    let blackPt = { row = 3; col = 3 }
    let whitePt = {row = 3; col = 2}
    let firstUpdateBoard = placeStone board (Player Black) blackPt
    let secondUpdate = placeStone firstUpdateBoard (Player White) whitePt
    let whiteStoneGroup = getStoneGroup secondUpdate.grid whitePt
    let removeGrid = removeStoneGroup secondUpdate.grid whiteStoneGroup.Value 5
    let expectedGrid = [
                         (blackPt, {color = Black; stones = Set.singleton (Stone blackPt); liberties = Set.ofList [Liberty {row = 2; col = 3}; Liberty { row = 3; col = 4}; Liberty { row = 4; col = 3}; Liberty {row = 3; col = 2}]})
                       ]
    Assert.Equal<Set<Point * StoneGroup>>(Set.ofSeq expectedGrid, Set.ofSeq removeGrid)

[<Fact>]
let ``grid is updated correctly when stone is captured`` () = 
    let board = { size = 5; grid = Seq.empty }
    let blackPt1 = { row = 3; col = 3 }
    let whitePt = {row = 3; col = 2}
    let blackPt2 = { row = 4; col = 2 }
    let blackPt3 = { row = 3; col = 1 }
    let blackPt4 = { row = 2; col = 2 }
    let firstUpdateBoard = placeStone board (Player Black) blackPt1
    let secondUpdate = placeStone firstUpdateBoard (Player White) whitePt
    let thirdUpdate = placeStone secondUpdate (Player Black) blackPt2
    let forthUpdate = placeStone thirdUpdate (Player Black) blackPt3
    let capturedUpdate = placeStone forthUpdate (Player Black) blackPt4
    let expectedGrid = [
                        (blackPt1, {color = Black; stones = Set.singleton (Stone blackPt1); liberties = Set.ofList [Liberty {row = 2; col = 3}; Liberty { row = 3; col = 4}; Liberty { row = 4; col = 3}; Liberty { row = 3; col = 2}]});
                        (blackPt2, {color = Black; stones = Set.singleton (Stone blackPt2); liberties = Set.ofList [Liberty {row = 3; col = 2}; Liberty { row = 5; col = 2}; Liberty {row = 4; col = 1}; Liberty { row = 4; col = 3}]});
                        (blackPt3, {color = Black; stones = Set.singleton (Stone blackPt3); liberties = Set.ofList [Liberty {row = 4; col = 1}; Liberty { row = 2; col = 1}; Liberty {row = 3; col = 2}]});
                        (blackPt4, {color = Black; stones = Set.singleton (Stone blackPt4); liberties = Set.ofList [Liberty {row = 2; col = 1}; Liberty { row = 2; col = 3}; Liberty {row = 1; col = 2}; Liberty { row = 3; col = 2}]})
                       ]
    Assert.Equal<Set<Point * StoneGroup>>(Set.ofSeq expectedGrid, Set.ofSeq capturedUpdate.grid)