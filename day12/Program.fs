let parseList (str: string) =
    str.Split ',' |> Array.map int |> List.ofArray

let parseLine (str: string) =
    match str.Split ' ' with
    | [| conditions; damaged |] -> conditions, parseList damaged
    | _ -> failwith $"Invalid line: %s{str}"

let unfoldStr n str =
    Seq.init n (fun _ -> str) |> String.concat "?"

let unfoldList n arr =
    Seq.init n (fun _ -> arr) |> Seq.concat |> Seq.toList

let unfold n (conditionStr, damagedList) =
    conditionStr |> unfoldStr n, damagedList |> unfoldList n

let rec consume springs damaged =
    if damaged = 0 then
        match springs with
        | [] -> [ springs ]
        | '#' :: _ -> []
        | _ :: tail -> [ tail ]
    else
        match springs with
        | [] -> []
        | '.' :: _ -> []
        | _ :: tail -> consume tail (damaged - 1)

let rec consumeGroup springs damaged =
    if damaged = 0 then
        [ springs ]

    else
        match springs with
        | [] -> []
        | '#' :: rest -> consume rest (damaged - 1)
        | '.' :: rest -> consumeGroup rest damaged
        | '?' :: rest ->
            let consumed = consume rest (damaged - 1)
            let notConsumed = consumeGroup rest damaged
            consumed @ notConsumed
        | _ -> failwith "Invalid char"


let run ((springs0: string), (pattern0: int list)) =

    let rec loop (cache: Map<_, _>) springs pattern =
        if cache.ContainsKey(springs, pattern) then
            cache
        else
            match pattern with
            | [] ->
                let result = if springs |> List.contains '#' then 0L else 1L
                cache |> Map.add (springs, pattern) result
            | damaged :: rest ->
                let consumedGroup = consumeGroup springs damaged

                let folder (cache, acc) possible =
                    let cache = loop cache possible rest
                    (cache, acc + cache[possible, rest])

                let (cache, n) = consumedGroup |> List.fold folder (cache, 0L)
                cache |> Map.add (springs, pattern) n

    let springs0 = springs0 |> List.ofSeq
    loop Map.empty springs0 pattern0 |> Map.find (springs0, pattern0)

let solve f =
    Utils.FileReading.readLines
    >> Seq.map parseLine
    >> Seq.map f
    >> Seq.map run
    >> Seq.sum

solve id "input.txt" |> printfn "%A"
solve (unfold 5) "input.txt" |> printfn "%A"
