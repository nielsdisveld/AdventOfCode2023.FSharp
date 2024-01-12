module Part2

open Common

let endsWith c (str: string) = str[2] = c

let rec path navigateNext start =
    let rec stepper acc =
        let current = acc |> List.tryHead |> Option.defaultValue start

        match navigateNext current with
        | next when acc |> List.contains next -> acc |> List.rev
        | next -> stepper (next :: acc)

    stepper []

let modEquation lst =
    let i = lst |> List.findIndex (endsWith 'Z')
    int64 i, int64 lst.Length // (a,b) represents a mod b

let run (instructions: string, nodes: Nodes) =
    let navigateNext = navigateMany nodes instructions

    let (solution, _) =
        nodes.Keys
        |> Seq.filter (endsWith 'A')
        |> Seq.map (fun start -> path navigateNext start)
        |> Seq.map modEquation
        |> Seq.reduce Utils.Math.chineseRemainder

    (1L + solution) * int64 instructions.Length

let solve = Utils.FileReading.readLines >> parseInput >> run
