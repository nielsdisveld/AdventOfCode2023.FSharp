module Part1

let parseLine (str: string) =
    str.Split ' '
    |> Array.skip 1
    |> Array.filter (fun s -> s <> "")
    |> Array.map int

let parse input =
    let parsed = input |> Seq.map parseLine
    Seq.zip (Seq.item 0 parsed) (Seq.item 1 parsed)

let distance t i = (t - i) * i

let isWinner (t, d) hold = distance t hold > d

let winners (t, d) =
    [| 1 .. t - 1 |] |> Array.filter (fun i -> distance t i > d) |> Array.length

let solve =
    Utils.FileReading.readLines >> parse >> Seq.map winners >> Seq.reduce (*)
