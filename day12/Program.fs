let parseList (str: string) = str.Split ',' |> Array.map int

let parseLine (str: string) =
    match str.Split ' ' with
    | [| conditions; damaged |] -> conditions, parseList damaged
    | _ -> failwith $"Invalid line: %s{str}"

let unfoldStr n str =
    Seq.init n (fun _ -> str) |> String.concat "?"

let unfoldList n arr =
    Seq.init n (fun _ -> arr) |> Seq.concat |> Seq.toArray

let unfold n (conditionStr, damagedList) =
    conditionStr |> unfoldStr n, damagedList |> unfoldList n

let count c str =
    str |> Seq.filter (fun c' -> c' = c) |> Seq.length

let update e' curr (ig, g, b, eb, eo, prev) =
    match prev, curr with
    | _, '#' -> (ig + 1, g, b + 1, eb + e', eo, curr)
    | '#', '.' -> (0, g + 1, b, eb, eo + e', curr)
    | '.', '.' -> (0, g, b, eb, eo + e', curr)
    | _ -> failwith $"Invalid combination of characters: {(prev, curr)}"

let rewrite ((str: string), (broken: int[])) =
    let totalBroken = broken |> Array.sum
    let currentBroken = str |> count '#'
    let unknowns = str |> count '?'
    let extraBroken = totalBroken - currentBroken
    let extraOperating = unknowns - extraBroken
    let broken = Array.append broken [| 0 |]
    let cumulativeBroken = broken |> Array.scan (+) 0

    let rec loop i (acc: _[]) =
        if acc.Length > 1000000 then
            (loop i acc[0..500000]) + (loop i acc[500001..])
        else if str.Length = i then
            acc |> Seq.filter (fun (_, _, b, _, _, _) -> b = totalBroken) |> Seq.length
        else
            let curr = str[i]

            let updated =
                match curr with
                | '?' ->
                    let left = acc |> Seq.map (update 1 '.')
                    let right = acc |> Seq.map (update 1 '#')
                    Seq.append left right
                | _ -> acc |> Seq.map (update 0 curr)

            updated
            |> Seq.filter (fun (ig, g, b, eb, eo, _) ->
                eb <= extraBroken
                && eo <= extraOperating
                && ig <= broken[g]
                && b >= cumulativeBroken[g])
            |> Seq.toArray
            |> loop (i + 1)

    loop 0 [| (0, 0, 0, 0, 0, '.') |]

let solve f =
    Utils.FileReading.readLines
    >> Seq.map parseLine
    >> Seq.map f
    >> Seq.map rewrite
    >> Seq.sum

let timer = System.Diagnostics.Stopwatch.StartNew()
solve id "input.txt" |> printfn "%A"
printfn "%A" timer.ElapsedMilliseconds
solve (unfold 5) "input.txt" |> printfn "%A"
printfn "%A" timer.ElapsedMilliseconds
