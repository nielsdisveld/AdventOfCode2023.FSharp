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
    if damaged = 0 then // In this case springs must be empty or there must be a '.'
        match springs with
        | [] -> [ springs ]
        | '#' :: _ -> []
        | _ :: tail -> [ tail ]
    else
        match springs with // When damage>0 there must a '#'
        | [] -> []
        | '.' :: _ -> []
        | _ :: tail -> consume tail (damaged - 1)

let rec findUntilDmgThenConsume springs damaged =
    // This method will only be called with damaged>0
    match springs with
    | [] -> []
    | '#' :: rest -> consume rest (damaged - 1)
    | '.' :: rest -> findUntilDmgThenConsume rest damaged
    | '?' :: rest ->
        let choseDmg = consume rest (damaged - 1)
        let choseOperational = findUntilDmgThenConsume rest damaged
        choseDmg @ choseOperational
    | _ -> failwith "Invalid char"


let run ((springs0: string), (pattern0: int list)) =

    let rec loop (cache: Map<_, _>) springs pattern =
        if cache.ContainsKey(springs, pattern) then
            cache, cache |> Map.find (springs, pattern)
        else
            match pattern with
            | [] ->
                let result = if springs |> List.contains '#' then 0L else 1L
                cache |> Map.add (springs, pattern) result, result
            | damaged :: rest ->
                let consumedGroup = findUntilDmgThenConsume springs damaged
                // 'consumedGroup' contains spring configurations that remain when the damage group
                // 'damaged' is removed from 'springs' in every possible valid way. Next we find all
                // possible configurations for these remainders with the remaining damage groups. We
                // add these results to the cache and the possible number of solutions for is now the
                // sum of all these possibilities.
                let folder (cache, acc) possible =
                    let (cache, n) = loop cache possible rest
                    (cache, acc + n)

                let (cache, n) = consumedGroup |> List.fold folder (cache, 0L)
                cache |> Map.add (springs, pattern) n, n

    let springs0 = springs0 |> List.ofSeq
    loop Map.empty springs0 pattern0 |> snd

let solve f =
    Utils.FileReading.readLines
    >> Seq.map parseLine
    >> Seq.map f
    >> Seq.map run
    >> Seq.sum

solve id "input.txt" |> printfn "%A"
solve (unfold 5) "input.txt" |> printfn "%A"
