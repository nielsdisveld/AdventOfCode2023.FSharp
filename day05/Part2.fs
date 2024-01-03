module Part2

type Range = int64 * int64 // A range is defined by a starting point and the length of the range

let difference (n, r) (n', r') = // The set difference of a range within a range
    if r' = 0L then
        [| (n, r) |]
    else
        [| (n, n' - n); (n' + r', r - r' - (n' - n)) |]

let mapRange (dest, source, range) (n, r) =
    let mappedn, mappedr =
        if (n + r < source) || (n > source + range) then
            (0L, 0L)
        elif n <= source then
            let r' = r - (source - n)
            source, min r' range
        else
            let r' = min (range - (n - source)) r
            (n, r')

    let unmapped = difference (n, r) (mappedn, mappedr)

    unmapped, (mappedn + (dest - source), mappedr)


let mapManyRanges (ranges: Range[]) mappingLine = // Map many ranges with one line of a map
    let unMapped, mapped = ranges |> Array.map (mapRange mappingLine) |> Array.unzip

    unMapped |> Array.collect id, mapped

let map (ranges: Range[]) mappingLines = // i.e. map many ranges with seed-to-soil map
    let folder (unmapped, mapped) mappingLine =
        let newUnmapped, newMapped = mapManyRanges unmapped mappingLine
        newUnmapped, Array.append mapped newMapped

    let unmapped, mapped = mappingLines |> List.fold folder (ranges, [||])

    Array.append mapped unmapped |> Array.filter (fun (_, r) -> r <> 0)

let parseRanges (str: string) =
    str.Split ' '
    |> Array.skip 1
    |> Array.map int64
    |> Array.windowed 2
    |> Array.mapi (fun i a -> i, (a[0], a[1]))
    |> Array.filter (fun (i, _) -> i % 2 = 0)
    |> Array.map snd

let almanac (inp: seq<string>) =
    let seedRanges = inp |> Seq.head |> parseRanges
    let maps = inp |> Seq.skip 1 |> Common.parseMapping |> List.rev
    (seedRanges, maps)

let mapSeeds (seeds, maps) = maps |> List.fold map seeds

let solve =
    Utils.FileReading.readLines >> almanac >> mapSeeds >> Array.minBy fst >> fst
