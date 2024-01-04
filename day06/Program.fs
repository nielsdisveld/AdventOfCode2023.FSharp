module Program

[<EntryPoint>]
let main _ =
    Part1.solve "input.txt" |> printfn "%A"
    Part2.solve "input.txt" |> printfn "%A"
    0
