module Part1

open Parsing

let isValid (v, color) =
    match color with
    | "red" -> v <= 12
    | "green" -> v <= 13
    | "blue" -> v <= 14
    | _ -> failwith "Invalid input: %s{str}"

let filterGame cubes = cubes |> Array.forall isValid

let solve =
    Utils.FileReading.readLines
    >> Seq.map parseLine
    >> Seq.filter (snd >> filterGame)
    >> Seq.sumBy fst
