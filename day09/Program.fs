let parseLine (str: string) = str.Split ' ' |> Array.map int

let differences (arr: int[]) =
    [| 1 .. arr.Length - 1 |] |> Array.map (fun i -> arr[i] - arr[i - 1])

let findZeroes (arr: int[]) =
    let rec stepper current acc =
        match current |> Array.forall (fun i -> i = 0) with
        | true -> acc
        | _ ->
            let last = current[current.Length - 1]
            let next = differences current
            stepper next (last :: acc)

    stepper arr []

let solveLine = findZeroes >> List.reduce (+)

let solve f =
    Utils.FileReading.readLines
    >> Seq.map parseLine
    >> Seq.map f // For part 2 we need to reverse all the arrays here
    >> Seq.map solveLine
    >> Seq.reduce (+)

"input.txt" |> solve id |> printfn "%i"
"input.txt" |> solve Array.rev |> printfn "%i"
