// Types
type Rating =
    { x: int64 * int64
      m: int64 * int64
      a: int64 * int64
      s: int64 * int64 }

type Subrating =
    | X
    | M
    | A
    | S

type Operator =
    | GreaterThan
    | LesserThan

type Decision =
    | Accepted
    | Rejected
    | Send of string
    | Condition of Subrating * Operator * int64 * Decision

// Parsing
let getSubrating s rating =
    match s with
    | X -> rating.x
    | M -> rating.m
    | A -> rating.a
    | S -> rating.s

let parseSubrating =
    function
    | "x" -> X
    | "m" -> M
    | "a" -> A
    | "s" -> S
    | str -> failwith $"Incorrect subrating: %s{str}"

let parseOutcome (str: string) =
    match str with
    | "A" -> Accepted
    | "R" -> Rejected
    | send -> Send send

let parseCondition (str: string) ifTrue =
    match str[0..0], str[1], str[2..] with
    | s, '<', i -> Condition(parseSubrating s, LesserThan, int64 i, ifTrue)
    | s, '>', i -> Condition(parseSubrating s, GreaterThan, int64 i, ifTrue)
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
        let x, m, a, s = int64 x, int64 m, int64 a, int64 s

        { x = x, x + 1L
          m = m, m + 1L
          a = a, a + 1L
          s = s, s + 1L }
    | _ -> failwith $"Invalid input: %s{str}"

// Solving
let applyCondition (subRating, operator, value) rating =
    match operator with
    | GreaterThan -> ((getSubrating subRating rating) |> fst) > value
    | LesserThan -> ((getSubrating subRating rating) |> fst) < value


let splitInterval x (x1, x2) =
    if x > x1 && x < x2 then
        [| x1, x; x, x2 |]
    else
        [| x1, x2 |]

let findAccepted (workflows: Map<string, Decision[]>) rating =
    let rec stepWorkflow i rating (workflow: Decision[]) =
        match workflow[i] with
        | Condition(subRating, operator, value, ifTrue) ->
            let split = if operator = GreaterThan then value + 1L else value

            let ratings =
                match subRating with
                | X -> rating.x |> splitInterval split |> Array.map (fun r -> { rating with x = r })
                | M -> rating.m |> splitInterval split |> Array.map (fun r -> { rating with m = r })
                | A -> rating.a |> splitInterval split |> Array.map (fun r -> { rating with a = r })
                | S -> rating.s |> splitInterval split |> Array.map (fun r -> { rating with s = r })

            ratings
            |> Seq.collect (fun rating ->
                if applyCondition (subRating, operator, value) rating then
                    Seq.singleton (rating, ifTrue)
                else
                    stepWorkflow (i + 1) rating workflow)
        | decision -> Seq.singleton (rating, decision)

    let rec loop rating name =
        stepWorkflow 0 rating workflows[name]
        |> Seq.collect (fun (rating, decision) ->
            match decision with
            | Accepted -> Seq.singleton rating
            | Rejected -> Seq.empty
            | Send send -> loop rating send
            | _ -> failwith "Oof")

    loop rating "in"

let score1 rating =
    (fst rating.x) + (fst rating.s) + (fst rating.a) + (fst rating.m)

let score2 rating =
    let inline l (i1, i2) = i2 - i1
    (l rating.x) * (l rating.m) * (l rating.a) * (l rating.s)

let parseInput inp =
    let input = inp |> Utils.FileReading.readLines
    let i = input |> Seq.findIndex (fun s -> s = "")
    let workflows = input |> Seq.take i |> Seq.map parseWorkflow |> Map.ofSeq
    let ratings = input |> Seq.skip (i + 1) |> Seq.map parseRating
    workflows, ratings

let solve1 workflows ratings =
    ratings
    |> Seq.filter (findAccepted workflows >> Seq.isEmpty >> not)
    |> Seq.sumBy score1

let solve2 workflows =
    let all =
        { x = (1, 4001)
          m = (1, 4001)
          a = (1, 4001)
          s = (1, 4001) }

    let ratings = findAccepted workflows all
    ratings |> Seq.sumBy score2

[<EntryPoint>]
let run _ =
    let workflows, ratings = parseInput "input.txt"
    solve1 workflows ratings |> printfn "Part1: %i"
    solve2 workflows |> printfn "Part2: %i"
    0
