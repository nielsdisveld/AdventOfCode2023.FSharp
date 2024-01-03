module Part1

let parseSeeds (str: string) =
    str.Split ' '
    |> Array.skip 1 // skip the "seeds:"
    |> Array.map int64

let almanac (inp: seq<string>) =
    let seeds = Seq.head inp |> parseSeeds
    let maps = inp |> Seq.skip 1 |> Common.parseMapping |> List.rev
    (seeds, maps)

let mapi i (dest, source, range) : int64 =
    if i >= source && i < source + range then
        dest - source + i
    else
        i

let mapSeed i (mapping: (int64 * int64 * int64) list) =
    let mapOpt = mapping |> List.tryFind (fun m -> mapi i m <> i)

    match mapOpt with
    | Some m -> mapi i m
    | None -> i

let mapSeeds ((seeds: int64[]), (mappings: ((int64 * int64 * int64) list) list)) =
    seeds |> Array.map (fun i -> mappings |> List.fold mapSeed i)

let solve = Utils.FileReading.readLines >> almanac >> mapSeeds >> Array.min
