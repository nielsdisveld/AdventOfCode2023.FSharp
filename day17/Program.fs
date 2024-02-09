let transform inp =
    let arr = inp |> Seq.map Seq.toArray |> Seq.toArray
    Array2D.init arr[0].Length arr.Length (fun x y -> ((arr[y][x]) |> int) - int '0')

let validTransitions n ((x, y), (dx, dy)) =
    if dx <> 0 && abs dx < (n - 6) then // For the case n=10 we cannot steer when speed < 4
        [| (dx + sign dx, 0) |]
    elif dx <> 0 then
        [| (dx + sign dx, 0); (0, 1); (0, -1) |]
    elif abs dy < (n - 6) then
        [| 0, dy + sign dy |]
    else
        [| 0, dy + sign dy; 1, 0; -1, 0 |]
    |> Array.map (fun (dx, dy) -> (x + sign dx, y + sign dy), (dx, dy))

let toTransitions n width height v =
    validTransitions n v
    |> Seq.filter (fun ((x, y), _) -> x >= 0 && y >= 0 && x < width && y < height)
    |> Seq.filter (fun (_, (dx, dy)) -> abs dx + abs dy <= n)
    |> Seq.toArray

let allTransitions n (arr: _[,]) =
    let width = arr.GetLength 0
    let height = arr.GetLength 1

    let speeds =
        Array.init n (fun i -> [| -(i + 1), 0; (i + 1), 0; 0, -(i + 1); 0, (i + 1) |])
        |> Array.collect id

    let grid = Seq.allPairs (Seq.init width id) (Seq.init height id)
    let states = Seq.allPairs grid speeds

    states |> Seq.map (fun v -> v, toTransitions n width height v) |> Map.ofSeq

let inline updateQueue (v, l) queue =
    match queue |> Array.tryFindIndex (fun (v', _) -> v' = v) with
    | Some i -> queue |> Array.updateAt i (v, l)
    | None -> queue |> Array.insertAt queue.Length (v, l)

type FindLoss(n, arr: _[,], edges) =
    let goal = (arr.GetLength 0) - 1, (arr.GetLength 1) - 1
    // These start positions force the next steps to be right or down with speed 1
    let start1 = ((0, 0), (-n, 0))
    let start2 = ((0, 0), (0, -n))

    let queue = [| (start1, 0); (start2, 0) |]

    let losses =
        edges
        |> Map.map (fun _ _ -> System.Int32.MaxValue)
        |> Map.add start1 0
        |> Map.add start2 0

    // Dijkstra with priority queue
    [<TailCall>]
    let rec loop (transitions: Map<_, _>) (losses: Map<_, _>) (queue: (_ * int)[]) =
        match queue |> Array.tryHead with
        | None -> losses |> Map.filter (fun (v, _) _ -> v = goal)
        | Some(v, _) ->
            let folder (losses: Map<_, _>, queue: (_ * int)[]) v2 =
                let (x, y), _ = v2
                let loss = min (losses[v] + arr[x, y]) losses[v2]
                losses.Add(v2, loss), queue |> updateQueue (v2, loss)

            let losses, queue =
                transitions.TryFind v
                |> Option.defaultValue [||]
                |> Seq.fold folder (losses, queue)

            let queue = queue |> Array.removeAt 0 |> Array.sortBy snd
            loop (transitions.Remove v) losses queue

    member _.run() = loop edges losses queue

let minLoss map = map |> Map.values |> Seq.min

let run n arr =
    let edges = arr |> allTransitions n

    FindLoss(n, arr, edges).run ()
    |> Map.filter (fun (_, (dx, dy)) _ -> abs dx + abs dy >= (n - 6))

let solve n =
    Utils.FileReading.readLines >> transform >> run n >> minLoss

solve 3 "input.txt" |> printfn "%A"
solve 10 "input.txt" |> printfn "%A"
