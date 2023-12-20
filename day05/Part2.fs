module Part2

let toArray (i, r) = [| i .. i + r - 1L |]

let parseRanges (str: string) =
    str.Split ' '
    |> Array.skip 1
    |> Array.map int64
    |> Array.windowed 2
    |> Array.mapi (fun i a -> i, (a[0], a[1]))
    |> Array.filter (fun (i, _) -> i % 2 = 0)
    |> Array.map snd
    |> Array.collect toArray

let alamanac (inp: seq<string>) =
    let seedRanges = inp |> Seq.head |> parseRanges
    printfn "%A" seedRanges.Length
    let maps = inp |> Seq.skip 1 |> Common.parseMapping |> List.rev
    (seedRanges, maps)

let solve = Utils.FileReading.readLines >> alamanac >> Common.mapSeeds >> Array.min
