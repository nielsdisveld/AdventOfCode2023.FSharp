let parseLine (str: string) = str.Split ' ' |> Seq.map int

let differences = Seq.pairwise >> Seq.map (fun (x, y) -> y - x)

let findZeroes sq =
    let rec stepper current acc =
        match current |> Seq.forall (fun i -> i = 0) with
        | true -> acc
        | _ ->
            let last = current |> Seq.last
            let next = differences current
            stepper next (last :: acc)

    stepper sq []

let solveLine = findZeroes >> Seq.reduce (+)

let solve f =
    Utils.FileReading.readLines
    >> Seq.map parseLine
    >> Seq.map f // For part 2 we need to reverse all the arrays here
    >> Seq.map solveLine
    >> Seq.reduce (+)

"input.txt" |> solve id |> printfn "%i"
"input.txt" |> solve Seq.rev |> printfn "%i"
