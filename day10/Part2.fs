module Part2

let updateState c topLeftIsInside =
    match c with
    | 'F'
    | '7'
    | '-' -> topLeftIsInside
    | 'L'
    | 'J'
    | '|' -> not topLeftIsInside
    | _ -> failwith "Invalid character."

let getInterior (tiles: char[,]) visited =
    let rec loop x y isInterior acc =
        if y >= tiles.GetLength 1 then
            acc
        elif x >= tiles.GetLength 0 then
            loop 0 (y + 1) false acc
        elif visited |> Set.contains (x, y) then
            let newState = updateState tiles[x, y] isInterior
            loop (x + 1) y newState acc
        elif isInterior then
            loop (x + 1) y isInterior (Set.add (x, y) acc)
        else
            loop (x + 1) y isInterior acc

    loop 0 0 false Set.empty

let solve tiles visited =
    visited |> getInterior tiles |> Set.count
