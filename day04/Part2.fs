module Part2

let x = [| 1; 2; 3 |]
let y = x[0..1]

let folder (prev: int[]) (i, c) =

    if c = 0 then
        prev[i] <- 1
    else
        let t = prev[i - c .. i - 1] |> Array.sum
        prev[i] <- t + 1


    prev

let total (correct: seq<int>) =
    let count = Seq.length correct
    let zeroes = Array.create count 0

    Seq.rev correct
    |> Seq.mapi (fun i c -> (i, c))
    |> Seq.fold folder zeroes
    |> Array.sum

let solve =
    Utils.FileReading.readLines
    >> Seq.map Common.parseLine
    >> Seq.map Common.correct
    >> total
