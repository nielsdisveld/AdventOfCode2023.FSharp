module Program

[<EntryPoint>]
let main _ =
    Common.solve Part1.handValue "input.txt" |> printfn "%A"
    Common.solve Part2.handValue "input.txt" |> printfn "%A"
    0
