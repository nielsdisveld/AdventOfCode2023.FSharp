// Types
type Rating =
    { x: int64
      m: int64
      a: int64
      s: int64 }

type Operator =
    | GreaterThan
    | LesserThan

type Decision =
    | Accepted
    | Rejected
    | Send of string
    | Condition of string * Operator * int64 * Decision

// Parsing
let getRating s rating =
    match s with
    | "x" -> rating.x
    | "m" -> rating.m
    | "a" -> rating.a
    | "s" -> rating.s
    | _ -> failwith $"Invalid category: %s{s}"

let parseOutcome (str: string) =
    match str with
    | "A" -> Accepted
    | "R" -> Rejected
    | send -> Send send

let parseCondition (str: string) ifTrue =
    match str[0..0], str[1], str[2..] with
    | s, '<', i -> Condition(s, LesserThan, int64 i, ifTrue)
    | s, '>', i -> Condition(s, GreaterThan, int64 i, ifTrue)
    | _ -> failwith $"Invalid condition: %s{str}"

let parseRule (str: string) =
    match str.Split ':' with
    | [| rule; outcome |] -> parseCondition rule (parseOutcome outcome)
    | [| outcome |] -> parseOutcome outcome
    | _ -> failwith $"Invalid rule: %s{str}"

let parseRules (str: string) = str.Split ',' |> Array.map parseRule

let parseWorkflow (str: string) =
    match str[0 .. str.Length - 2].Split '{' with
    | [| name; rules |] -> name, parseRules rules
    | _ -> failwith $"Invalid workflow: %s{str}"

let parseRating (str: string) =
    match str[1 .. str.Length - 2].Split ',' |> Array.map (fun str -> str.Split '=') with
    | [| [| "x"; x |]; [| "m"; m |]; [| "a"; a |]; [| "s"; s |] |] ->
        { x = int64 x
          m = int64 m
          a = int64 a
          s = int64 s }
    | _ -> failwith $"Invalid input: %s{str}"

// Solving
let applyCondition (subRating, operator, value) rating =
    match operator with
    | GreaterThan -> (getRating subRating rating) > value
    | LesserThan -> (getRating subRating rating) < value


let splitInterval x1 x2 x =
    if x > x1 && x < x2 then
        [| x1, x; x, x2 |]
    else
        [| x1, x2 |]

let findAccepted x m a s (workflows: Map<string, Decision[]>) =
    let rec stepWorkflow i (x1, x2) (m1, m2) (a1, a2) (s1, s2) (workflow: Decision[]) =
        match workflow[i] with
        | Condition(subRating, operator, value, ifTrue) ->
            let split = if operator = GreaterThan then value + 1L else value

            match subRating with
            | "x" ->
                splitInterval x1 x2 split
                |> Seq.collect (fun (i1, i2) ->
                    if applyCondition ("x", operator, value) { x = i1; m = m1; a = a1; s = s1 } then
                        Seq.singleton (((i1, i2), (m1, m2), (a1, a2), (s1, s2)), ifTrue)
                    else
                        stepWorkflow (i + 1) (i1, i2) (m1, m2) (a1, a2) (s1, s2) workflow)
            | "m" ->
                splitInterval m1 m2 split
                |> Seq.collect (fun (i1, i2) ->
                    if applyCondition ("m", operator, value) { x = x1; m = i1; a = a1; s = s1 } then
                        Seq.singleton (((x1, x2), (i1, i2), (a1, a2), (s1, s2)), ifTrue)
                    else
                        stepWorkflow (i + 1) (x1, x2) (i1, i2) (a1, a2) (s1, s2) workflow)
            | "a" ->
                splitInterval a1 a2 split
                |> Seq.collect (fun (i1, i2) ->
                    if applyCondition ("a", operator, value) { x = x1; m = m1; a = i1; s = s1 } then
                        Seq.singleton (((x1, x2), (m1, m2), (i1, i2), (s1, s2)), ifTrue)
                    else
                        stepWorkflow (i + 1) (x1, x2) (m1, m2) (i1, i2) (s1, s2) workflow)
            | "s" ->
                splitInterval s1 s2 split
                |> Seq.collect (fun (i1, i2) ->
                    if applyCondition ("s", operator, value) { x = x1; m = m1; a = a1; s = i1 } then
                        Seq.singleton (((x1, x2), (m1, m2), (a1, a2), (i1, i2)), ifTrue)
                    else
                        stepWorkflow (i + 1) (x1, x2) (m1, m2) (a1, a2) (i1, i2) workflow)
            | _ -> failwith "Oof"
        | decision -> Seq.singleton (((x1, x2), (m1, m2), (a1, a2), (s1, s2)), decision)

    let rec loop (x1, x2) (m1, m2) (a1, a2) (s1, s2) name =
        stepWorkflow 0 (x1, x2) (m1, m2) (a1, a2) (s1, s2) workflows[name]
        |> Seq.collect (fun ((x, m, a, s), decision) ->
            match decision with
            | Accepted -> Seq.singleton (x, m, a, s)
            | Rejected -> Seq.empty
            | Send send -> loop x m a s send
            | _ -> failwith "Oof")

    loop x m a s "in"

let score1 rating =
    rating.x + rating.s + rating.a + rating.m

let score2 ((x1, x2), (m1, m2), (a1, a2), (s1, s2)) =
    (x2 - x1) * (m2 - m1) * (a2 - a1) * (s2 - s1)

let parseInput inp =
    let input = inp |> Utils.FileReading.readLines
    let i = input |> Seq.findIndex (fun s -> s = "")
    let workflows = input |> Seq.take i |> Seq.map parseWorkflow |> Map.ofSeq
    let ratings = input |> Seq.skip (i + 1) |> Seq.map parseRating
    workflows, ratings

let solve1 workflows ratings =
    ratings
    |> Seq.filter (fun { x = x; m = m; a = a; s = s } ->
        findAccepted (x, x + 1L) (m, m + 1L) (a, a + 1L) (s, s + 1L) workflows
        |> Seq.isEmpty
        |> not)
    |> Seq.sumBy score1

let solve2 workflows =
    let ratings = findAccepted (1, 4001) (1, 4001) (1, 4001) (1, 4001) workflows
    ratings |> Seq.sumBy score2

[<EntryPoint>]
let run _ =
    let workflows, ratings = parseInput "input.txt"
    solve1 workflows ratings |> printfn "Part1: %i"
    solve2 workflows |> printfn "Part2: %i"
    0
