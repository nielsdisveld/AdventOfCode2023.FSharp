module Part2

let folder (r, g, b) cube =
    match cube with
    | (v, "red") -> (max r v, g, b)
    | (v, "green") -> (r, max g v, b)
    | (v, "blue") -> (r, g, max b v)
    | _ -> failwith "%A{cube}"

let powerSet cubes =
    let (r, g, b) = cubes |> Array.fold folder (0, 0, 0)
    r * g * b

let solve =
    Utils.FileReading.readLines
    >> Seq.map Parsing.parseLine
    >> Seq.map (snd >> powerSet)
    >> Seq.sum
