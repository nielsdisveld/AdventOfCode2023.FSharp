module Common

type Direction =
    | N
    | E
    | S
    | W

let opposite =
    function
    | N -> S
    | E -> W
    | S -> N
    | W -> E

let toRelPos =
    function
    | N -> 0, -1
    | E -> 1, 0
    | S -> 0, 1
    | W -> -1, 0

let connections =
    function
    | '|' -> [| N; S |]
    | '-' -> [| E; W |]
    | '7' -> [| W; S |]
    | 'J' -> [| N; W |]
    | 'L' -> [| N; E |]
    | 'F' -> [| S; E |]
    | _ -> [||]
    >> Set.ofArray

let adjacent = connections >> Set.map toRelPos

let findStart (tiles: char[,]) =
    match tiles |> Utils.Array2D.findIndex 'S' with
    | Some(x, y) -> x, y
    | _ -> failwith "Cannot find start tile."

let add (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let setStartPipe (tiles: char[,]) =
    let start = findStart tiles

    let startConnections =
        [| N; E; S; W |]
        |> Array.map (fun d -> d, d |> toRelPos |> add start) // i.e. N maps to N, (5,7)
        |> Array.filter (fun (d, (x, y)) -> tiles[x, y] |> connections |> Set.contains (opposite d))
        |> Array.map fst
        |> Set.ofArray

    let startPipe =
        [| '|'; '-'; '7'; 'J'; 'L'; 'F' |]
        |> Array.find (fun c -> connections c |> Set.isSuperset startConnections)

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
    let rec loop i visited currents =
        let adjacents =
            currents
            |> Seq.map (fun (x, y) -> tiles[x, y] |> adjacent |> Set.map (add (x, y)))
            |> Set.unionMany

        let nexts = Set.difference adjacents visited

        if nexts |> Set.isEmpty then
            i, visited
        else
            loop (i + 1) (Set.union visited nexts) nexts

    loop 0 (Set.singleton start) (Set.singleton start)

let getTiles = Utils.FileReading.readLines >> transform >> setStartPipe
