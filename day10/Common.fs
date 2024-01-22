module Common

// Wind directions:
let n, e, s, w = (0, -1), (1, 0), (0, 1), (-1, 0)

let opposite (x, y) = (-x, -y)
let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let connections =
    function
    | '|' -> [| n; s |]
    | '-' -> [| e; w |]
    | '7' -> [| w; s |]
    | 'J' -> [| n; w |]
    | 'L' -> [| n; e |]
    | 'F' -> [| s; e |]
    | _ -> [||]
    >> Set.ofArray

let adjacents pipe (x, y) =
    pipe |> connections |> Set.map (add (x, y))

let findStart (tiles: char[,]) =
    match tiles |> Utils.Array2D.findIndex 'S' with
    | Some(x, y) -> x, y
    | _ -> failwith "Cannot find start tile."

let setStartPipe (tiles: char[,]) =
    let start = findStart tiles

    let startConnections =
        [| n; e; s; w |]
        |> Array.map (fun d -> d, d |> add start)
        |> Array.filter (fun (d, (x, y)) -> tiles[x, y] |> connections |> Set.contains (opposite d))
        |> Array.map fst
        |> Set.ofArray

    let startPipe =
        [| '|'; '-'; '7'; 'J'; 'L'; 'F' |]
        |> Array.find (fun c -> connections c |> Set.isSuperset startConnections) // See which pipe 'fits' the start tile

    let updated = tiles |> Array2D.map (fun c -> if c = 'S' then startPipe else c)

    start, updated

let transform input =
    let arr = input |> Seq.map Seq.toArray |> Seq.toArray
    let width = 2 + arr[0].Length
    let height = 2 + arr.Length

    Array2D.init width height (fun x y ->
        if x = 0 || y = 0 || x = width - 1 || y = height - 1 then
            '.' // Creates a boundary of dots so we don't really have to consider out of bounds exceptions
        else
            arr[y - 1][x - 1])

let getLoop start (tiles: char[,]) =
    let rec loop i visited heads =
        let adjToHeads =
            heads |> Seq.map (fun (x, y) -> adjacents tiles[x, y] (x, y)) |> Set.unionMany

        let nexts = Set.difference adjToHeads visited

        if nexts |> Set.isEmpty then
            i, visited
        else
            loop (i + 1) (Set.union visited nexts) nexts

    loop 0 (Set.singleton start) (Set.singleton start)

let getTiles = Utils.FileReading.readLines >> transform >> setStartPipe
