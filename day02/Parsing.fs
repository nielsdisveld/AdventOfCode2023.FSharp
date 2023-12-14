module Parsing

let parseId (str: string) =
    match str.Split ' ' with
    | [| _; i |] -> int i
    | _ -> failwith "Invalid id: %s{str}"

let parseCubes (str: string) =
    match str.Split " " with
    | [| v; color |] -> int v, color
    | _ -> failwith "Invalid cubes: %s{str}"

let parseGrabs (str: string) = str.Split ", " |> Array.map parseCubes

let parseSets (str: string) =
    str.Split "; " |> Array.collect parseGrabs

let parseLine (str: string) =
    match str.Split ": " with
    | [| i; sets |] -> parseId i, parseSets sets
    | _ -> failwith "Invalid input: %s{str}"
