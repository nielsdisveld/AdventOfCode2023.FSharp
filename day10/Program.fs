module Program

[<EntryPoint>]
let main _ =
    let start, tiles = Common.getTiles "input.txt"
    let n, visited = Common.getLoop start tiles
    printfn "Farthest tile from starting point: %i" n // Part1
    Part2.solve tiles visited |> printfn "Number of tiles enclosed by the loop: %A" // Part2
    0
