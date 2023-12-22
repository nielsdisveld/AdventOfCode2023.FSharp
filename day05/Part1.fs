module Part1

let parseSeeds (str: string) =
    str.Split ' '
    |> Array.skip 1 // skip the "seeds:"
    |> Array.map int64

let almanac (inp: seq<string>) =
    let seeds = Seq.head inp |> parseSeeds
    let maps = inp |> Seq.skip 1 |> Common.parseMapping |> List.rev
    (seeds, maps)

let solve = Utils.FileReading.readLines >> almanac >> Common.mapSeeds >> Array.min
