let n, e, s, w = (0, -1), (1, 0), (0, 1), (-1, 0)
let (+) (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

let transform inp =
    let arr = inp |> Seq.map (Seq.toArray) |> Seq.toArray
    Array2D.init arr[0].Length arr.Length (fun x y -> arr[y][x])

let next (c: char) p direction =
    match c, direction with
    | '/', (0, -1)
    | '\\', (0, 1) -> [| e |]
    | '\\', (0, -1)
    | '/', (0, 1) -> [| w |]
    | '/', (1, 0)
    | '\\', (-1, 0) -> [| n |]
    | '\\', (1, 0)
    | '/', (-1, 0) -> [| s |]
    | '-', (0, -1)
    | '-', (0, 1) -> [| e; w |]
    | '|', (1, 0)
    | '|', (-1, 0) -> [| n; s |]
    | _ -> [| direction |]
    |> Seq.map (fun d -> p + d, d)

let isInside (arr: _[,]) (x, y) =
    x >= 0 && x < arr.GetLength 0 && y >= 0 && y < arr.GetLength 1

let energized (arr: _[,]) start =
    let rec loop visited heads =
        if Set.isEmpty heads then
            visited |> Set.map fst |> Set.count
        else
            let visited = Set.union visited heads

            let heads =
                heads
                |> Seq.collect (fun ((x, y), d) -> next arr[x, y] (x, y) d)
                |> Seq.filter (fst >> isInside arr)
                |> Seq.filter (visited.Contains >> not)
                |> Set.ofSeq

            loop visited heads

    loop Set.empty (Set.singleton start)

let findMax (arr: _[,]) =
    let width = arr.GetLength 0
    let height = arr.GetLength 1

    let top = Seq.init width (fun i -> (i, 0), s)
    let bottom = Seq.init width (fun i -> (i, height - 1), n)
    let left = Seq.init height (fun i -> (0, i), e)
    let right = Seq.init height (fun i -> (width - 1, i), w)

    Seq.concat [| top; bottom; left; right |] |> Seq.map (energized arr) |> Seq.max

let part1 =
    Utils.FileReading.readLines
    >> transform
    >> (fun arr -> energized arr ((0, 0), e))

let part2 = Utils.FileReading.readLines >> transform >> findMax

part1 "input.txt" |> printfn "%A"
part2 "input.txt" |> printfn "%A"
