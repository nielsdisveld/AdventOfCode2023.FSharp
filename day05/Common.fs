module Common

let parseMap (str: string) =
    match str.Split ' ' with
    | [| dest; source; range |] -> (int64 dest, int64 source, int64 range)
    | _ -> failwith $"Invalid line: %s{str}"

let hasDigits (str: string) =
    str.Length > 0 && Array.contains str[0] [| '0' .. '9' |]

let parseMapping (inp: seq<string>) =
    let folder acc (str: string) =
        match (str, acc, hasDigits str) with
        | "", _, _ -> [] :: acc
        | _, _, false -> acc
        | _, h :: acc', true -> ((parseMap str) :: h) :: acc'
        | _ -> failwith $"Invalid line: %s{str}"

    inp |> Seq.fold folder []

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
