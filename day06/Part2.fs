module Part2

open System

let parseLine (str: string) =
    str.Split ' '
    |> Array.skip 1
    |> Array.filter (fun s -> s <> "")
    |> Array.reduce (+)
    |> float

let toTuple seq = Seq.item 0 seq, Seq.item 1 seq

let zeroes (a: float, b: float, c: float) = // Solutions of ax^2+bx+c=0
    let d = Math.Sqrt(Math.Pow(b, 2.0) - 4.0 * a * c)
    (-b - d) / (2.0 * a), (-b + d) / (2.0 * a)

let winningRange (t, d) =
    let zero1, zero2 = zeroes (1, -t, d) // Solving (t-i)*i=d where i is button pressed reduces to solving i^2-ti+d=0
    let adjusted = min t (Math.Floor zero2) // Make sure that the solutions don't exceed t
    adjusted - (Math.Ceiling zero1) // Number of solutions is equal to the integer distance between the rounded zeroes

let solve =
    Utils.FileReading.readLines >> Seq.map parseLine >> toTuple >> winningRange
