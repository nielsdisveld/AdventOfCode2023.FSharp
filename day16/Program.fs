type Direction =
    | N
    | W
    | S
    | E

let transform inp =
    let arr = inp |> Seq.map (Seq.toArray) |> Seq.toArray
    Array2D.init arr[0].Length arr.Length (fun x y -> arr[y][x])

let move (x, y) direction =
    match direction with
    | N -> (x, y - 1)
    | E -> (x + 1, y)
    | S -> (x, y + 1)
    | W -> (x - 1, y)

let next (c: char) (direction: Direction) =
    match c, direction with
    | '/', N
    | '\\', S -> [| E |]
    | '\\', N
    | '/', S -> [| W |]
    | '/', E
    | '\\', W -> [| N |]
    | '\\', E
    | '/', W -> [| S |]
    | '-', N
    | '-', S -> [| E; W |]
    | '|', E
    | '|', W -> [| N; S |]
    | _ -> [| direction |]

let isInside (arr: _[,]) (x, y) =
    x >= 0 && x < arr.GetLength 0 && y >= 0 && y < arr.GetLength 1

let beam (arr: _[,]) start =
    let rec loop visited heads =
        if Set.isEmpty heads then
            visited |> Set.map fst |> Set.count
        else
            let visited = Set.union visited heads

            let nextHeads =
                heads
                |> Seq.collect (fun ((x, y), d) -> next arr[x, y] d |> Seq.map (fun d' -> (x, y), d'))
                |> Seq.map (fun ((x, y), d) -> move (x, y) d, d)
                |> Seq.filter (fst >> isInside arr)
                |> Seq.filter (visited.Contains >> not)
                |> Set.ofSeq

            loop visited nextHeads

    loop Set.empty (Set.singleton start)

let findMax (arr: _[,]) =
    let width = arr.GetLength 0
    let height = arr.GetLength 1
    let top = Seq.init width (fun i -> (i, 0), S)
    let bottom = Seq.init width (fun i -> (i, height - 1), N)
    let left = Seq.init height (fun i -> (0, i), E)
    let right = Seq.init height (fun i -> (width - 1, i), W)

    Seq.concat [| top; bottom; left; right |] |> Seq.map (beam arr) |> Seq.max

let part1 =
    Utils.FileReading.readLines >> transform >> (fun arr -> beam arr ((0, 0), E))

let part2 = Utils.FileReading.readLines >> transform >> findMax
part1 "input.txt" |> printfn "%A"
part2 "input.txt" |> printfn "%A"
