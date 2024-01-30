let transform input =
    let arr = input |> Seq.rev |> Seq.map Seq.toArray |> Seq.toArray
    let width = 2 + arr[0].Length
    let height = 2 + arr.Length

    Array2D.init width height (fun x y ->
        if x = 0 || y = 0 || x = width - 1 || y = height - 1 then
            '#' // Creates a boundary of cube-shaped rocks
        else
            arr[y - 1][x - 1])

let rocks (arr: _[,]) =
    let rec loop x y cubes rounds =
        if y >= arr.GetLength 1 then
            (cubes, rounds)
        elif x >= arr.GetLength 0 then
            loop 0 (y + 1) cubes rounds
        else
            match arr[x, y] with
            | '#' -> loop (x + 1) y ((x, y) :: cubes) rounds
            | 'O' -> loop (x + 1) y cubes ((x, y) :: rounds)
            | _ -> loop (x + 1) y cubes rounds

    let c, b = loop 0 0 [] []
    set c, set b

let load rounds = rounds |> Seq.sumBy snd

let tiltS cubes rounds =
    let nearestCube (x, y) =
        cubes |> Seq.filter (fun (a, b) -> a = x && b < y) |> Seq.maxBy snd

    rounds
    |> Seq.countBy nearestCube
    |> Seq.collect (fun ((x, y), l) -> Seq.init l (fun i -> (x, y + 1 + i)))

let tiltW cubes rounds =
    let nearestCube (x, y) =
        cubes |> Seq.filter (fun (a, b) -> b = y && a < x) |> Seq.maxBy fst

    rounds
    |> Seq.countBy nearestCube
    |> Seq.collect (fun ((x, y), l) -> Seq.init l (fun i -> (x + 1 + i, y)))

let tiltN cubes rounds =
    let nearestCube (x, y) =
        cubes |> Seq.filter (fun (a, b) -> a = x && b > y) |> Seq.minBy snd

    rounds
    |> Seq.countBy nearestCube
    |> Seq.collect (fun ((x, y), l) -> Seq.init l (fun i -> (x, y - 1 - i)))

let tiltE cubes rounds =
    let nearestCube (x, y) =
        cubes |> Seq.filter (fun (a, b) -> b = y && a > x) |> Seq.minBy fst

    rounds
    |> Seq.countBy nearestCube
    |> Seq.collect (fun ((x, y), l) -> Seq.init l (fun i -> (x - 1 - i, y)))

let cycle cubes =
    tiltN cubes >> tiltW cubes >> tiltS cubes >> tiltE cubes

let cyclen n (cubes, rounds) =
    let rec loop i (cache: Map<_, _>) (cacheInv: Map<_, _>) rounds =

        if i = n then
            rounds
        elif cache.ContainsKey rounds then
            let j = cache[rounds]
            let r = (n - i) % (i - j)
            cacheInv[j + r]
        else
            let cycled = rounds |> cycle cubes |> set
            let cache = cache.Add(rounds, i)
            let cacheInv = cacheInv.Add(i, rounds)
            cycled |> loop (i + 1) cache cacheInv

    loop 0 Map.empty Map.empty rounds

let part1 =
    Utils.FileReading.readLines
    >> transform
    >> rocks
    >> (fun (c, r) -> tiltN c r)
    >> load

let part2 =
    Utils.FileReading.readLines >> transform >> rocks >> cyclen 1000000000 >> load

part1 "input.txt" |> printfn "%A"
part2 "input.txt" |> printfn "%A"
