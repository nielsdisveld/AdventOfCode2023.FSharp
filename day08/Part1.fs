module Part1

open Common

let rec navigateUntilGoal n (instructions: string) (nodes: Nodes) (current: string) (goal: string) =
    match navigateMany nodes instructions current with
    | node when node = goal -> n + 1
    | node -> navigateUntilGoal (n + 1) instructions nodes node goal

let run (instructions: string, nodes: Nodes) =
    let start = nodes.Keys |> Seq.head
    let goal = nodes.Keys |> Seq.last
    let n = navigateUntilGoal 0 instructions nodes start goal
    n * instructions.Length

let solve = Utils.FileReading.readLines >> parseInput >> run
