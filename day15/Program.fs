let parseLine (str: string) = str.Split ','

let hash (str: string) =
    let folder acc c =
        acc |> (fun a -> a + int c) |> (fun a -> 17 * a) |> (fun a -> a % 256)

    str |> Seq.fold folder 0

let solve =
    Utils.FileReading.readLines
    >> Seq.head
    >> parseLine
    >> Array.map hash
    >> Array.sum

solve "input.txt" |> printfn "%A"
