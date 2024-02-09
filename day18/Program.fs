let up, right, down, left = (0L, -1L), (1L, 0L), (0L, 1L), (-1L, 0L)
let inline (+.) (x1, y1) (x2, y2) = x1 + x2, y1 + y2
let inline sign i = System.Int64.Sign i |> int64

let parseDirection (str: string) =
    match str with
    | "U"
    | "3" -> up
    | "R"
    | "0" -> right
    | "D"
    | "1" -> down
    | "L"
    | "2" -> left
    | x -> failwith $"Invalid direction: %s{x}"

let parseHex (str: string) =
    let numbers =
        Array.append [| '0' .. '9' |] [| 'a' .. 'f' |]
        |> Seq.mapi (fun i c -> c, int64 i)
        |> Map.ofSeq

    let folder (acc, i) n = acc + (n * pown 16L i), i + 1

    str
    |> Seq.map (fun c -> numbers.Item c)
    |> Seq.rev
    |> Seq.fold folder (0L, 0)
    |> fst

let parseLine (str: string) =
    match str.Split ' ' with
    | [| d; l; h |] ->
        (parseDirection d, int64 l), (parseDirection h[h.Length - 2 .. h.Length - 2], parseHex h[2 .. h.Length - 3])
    | _ -> failwith $"Invalid input: %s{str}"


let getTrench inp =
    let folder ((x, y), _) ((dx, dy), i: int64) =
        (x + i * (sign dx), y + i * (sign dy)), (dx, dy)

    inp
    |> Seq.scan folder ((1L, 1L), (0L, 0L))
    |> Seq.pairwise
    |> Seq.filter (fun ((_, _), (_, d)) -> d = up || d = down)
    |> Seq.map (fun ((p1, _), (p2, d)) -> if d = up then p2, p1 else p1, p2)

let countInterior (trench: seq<(int64 * int64) * (int64 * int64)>) =
    let ys =
        trench
        |> Seq.collect (fun ((_, y1), (_, y2)) -> [| y1; y2 |])
        |> Seq.distinct
        |> Seq.sort
        |> Seq.pairwise

    let lastLine = ys |> Seq.last |> snd |> (fun y -> y, y + 1L)
    let ys = Seq.append ys [ lastLine ]
    let trench = trench |> Seq.sortBy (fst >> fst)

    let countLine y =
        let rec loop acc outsideOnLeft onBorder isInside x =
            let line =
                trench |> Seq.tryFind (fun ((x1, y1), (_, y2)) -> y >= y1 && y <= y2 && x1 > x)

            match line with
            | None -> acc
            | Some((x1, y1), (_, y2)) ->
                if isInside then
                    let acc = acc + x1 - x

                    if y = y1 then loop acc false true false x1
                    elif y = y2 then loop acc true true false x1
                    else loop acc false false false x1
                elif onBorder then
                    let acc = acc + x1 - x

                    if (outsideOnLeft && y = y2) || (not outsideOnLeft && y = y1) then
                        loop acc false false true x1
                    else
                        loop acc false false false x1
                elif y = y1 then
                    loop (acc + 1L) true true false x1
                elif y = y2 then
                    loop (acc + 1L) false true false x1
                else
                    loop (acc + 1L) false false true x1

        loop 0L false false false System.Int64.MinValue

    let countStrip (y1, y2) =
        if y1 = y2 then
            0L
        elif y1 + 1L = y2 then
            countLine y1
        else
            (countLine y1) + ((y2 - y1 - 1L) * (countLine (y1 + 1L)))

    ys |> Seq.map (countStrip >> int64) |> Seq.sum


[<EntryPoint>]
let run _ =
    let input1, input2 =
        "input.txt"
        |> Utils.FileReading.readLines
        |> Seq.map parseLine
        |> Seq.toArray
        |> Array.unzip

    input1 |> getTrench |> countInterior |> printfn "%i"
    input2 |> getTrench |> countInterior |> printfn "%i"
    0
