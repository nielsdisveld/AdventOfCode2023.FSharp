module Part2

type Gear = int * int

let gearsOnRow (j: int) (row: string) : Gear[] =
    row.ToCharArray()
    |> Array.mapi (fun i c -> (i, j), c)
    |> Array.filter (fun (_, c) -> c = '*')
    |> Array.map fst

let getGears (rows: seq<string>) =
    rows |> Seq.mapi gearsOnRow |> Seq.collect id

let isConnected ((x, y): Gear) (((i, j), str: string): Common.Part) =
    abs (y - j) < 2 && x >= i - 1 && x <= i + str.Length

let gearRatio parts gear =
    let filtered = parts |> Seq.filter (isConnected gear) |> Seq.map (snd >> int)

    match Seq.length filtered with
    | 2 -> (Seq.item 0 filtered) * (Seq.item 1 filtered)
    | _ -> 0

let run rows =
    let gears = getGears rows
    let parts = Common.getParts rows

    gears |> Seq.map (gearRatio parts) |> Seq.sum

let solve = Utils.FileReading.readLines >> run
