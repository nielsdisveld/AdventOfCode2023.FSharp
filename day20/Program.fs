open Utils.Queue

// Types
type FlipFlop = bool

type Conjuction = Map<string, bool>

type Module =
    | FlipFlop of FlipFlop
    | Conjuction of Conjuction
    | Broadcaster

    member this.isConjuction =
        match this with
        | Conjuction _ -> true
        | _ -> false

type Pulse = string * bool * string

// Parsing
let parseDestinations (str: string) = str.Split ", "

let parseLine (str: string) =
    match str.Split " -> " with
    | [| s1; s2 |] ->
        match s1[0] with
        | '%' -> s1[1..], (FlipFlop false, parseDestinations s2)
        | '&' -> s1[1..], (Conjuction Map.empty, parseDestinations s2)
        | _ -> s1, (Broadcaster, parseDestinations s2)
    | _ -> failwith "Oof"

let finalizeConjuction (map: Map<string, (Module * _[])>) name (_, d) =
    let folder inputs modName (_, destinations) =
        if destinations |> Array.contains name then
            modName :: inputs
        else
            inputs

    let inputs =
        map
        |> Map.fold folder []
        |> Seq.distinct
        |> Seq.map (fun d -> d, false)
        |> Map.ofSeq

    map |> Map.add name (Conjuction inputs, d)


let parseInput inp =
    let map = inp |> Seq.map parseLine |> Map.ofSeq

    map
    |> Map.filter (fun _ (modType, _) -> modType.isConjuction)
    |> Map.fold finalizeConjuction map

let getInput = Utils.FileReading.readLines >> parseInput

// Solving
// Part1
let inline nextPulses source signal targets =
    targets |> Seq.map (fun t -> source, signal, t)

let inline handlePulse (map: Map<string, _>) (source, signal, target) =
    match map.TryFind target with
    | None -> map, Seq.empty
    | Some(FlipFlop s, ds) ->
        if signal then
            map, Seq.empty
        else
            let map = map.Add(target, (FlipFlop(not s), ds))
            map, ds |> nextPulses target (not s)
    | Some(Conjuction switches, ds) ->
        let switches = switches.Add(source, signal)
        let map = map.Add(target, (Conjuction switches, ds))

        if (switches |> Map.values |> Seq.forall id) then
            map, ds |> nextPulses target false
        else
            map, ds |> nextPulses target true
    | Some(Broadcaster, ds) -> map, ds |> nextPulses target signal

let inline pressButton (l, h, map) =

    let rec loop l h map (queue: Queue<Pulse>) =
        if queue.IsEmpty then
            l, h, map
        else
            let (source, signal, target), queue = queue.Dequeue
            let map, pulses = handlePulse map (source, signal, target)
            let l, h = if signal then l, h + 1 else l + 1, h
            loop l h map (queue.EnqueueMany pulses)


    let pulse: Pulse = "", false, "broadcaster"
    loop l h map (Queue.Empty.Enqueue pulse)

let pressButtonN n (map: Map<_, _>) =
    [| 1..n |] |> Seq.fold (fun state _ -> pressButton state) (0, 0, map)

let score (l, h, _) = l * h

// Part2
let inline pressButtonPart2 (map: Map<string, _>) conjName switchName =
    let rec loop map (queue: Queue<Pulse>) =
        if queue.IsEmpty then
            false, map
        else
            let (source, signal, target), queue = queue.Dequeue
            let map, pulses = handlePulse map (source, signal, target)

            match map[conjName] with
            | Conjuction switches, _ when switches[switchName] -> true, map
            | _ -> loop map (queue.EnqueueMany pulses)



    let pulse: Pulse = "", false, "broadcaster"
    loop map (Queue.Empty.Enqueue pulse)

let pressUntilHigh map conjName switchName =
    let rec loop i map =
        let isHigh, map = pressButtonPart2 map conjName switchName
        if isHigh then i else loop (i + 1L) map

    loop 1 map


let findlastConjBeforeRx map =
    let rec loop moduleName =
        let prev = map |> Map.findKey (fun _ (_, arr) -> arr |> Array.contains moduleName)

        match map |> Map.find prev with
        | FlipFlop _, _ -> loop prev
        | (Conjuction switches, _) -> prev, switches.Keys
        | _ -> failwith "Oof"

    loop "rx"


// Running
let part1 = pressButtonN 1000 >> score

let part2 input =
    let conjuctionName, switches = input |> findlastConjBeforeRx
    switches |> Seq.map (pressUntilHigh input conjuctionName) |> Seq.reduce (*)

[<EntryPoint>]
let run _ =
    let input = "input.txt" |> getInput
    input |> part1 |> printfn "%A"
    input |> part2 |> printfn "%i"
    0
