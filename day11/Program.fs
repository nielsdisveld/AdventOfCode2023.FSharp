﻿let uniquePairs sq =
    sq
    |> Seq.allPairs sq
    |> Seq.filter (fun (a, b) -> a <> b)
    |> Seq.distinctBy (fun (a, b) -> min a b, max a b)

let findGalaxies (arr: char[,]) =
    let rec loop x y acc =
        if y >= arr.GetLength 1 then acc
        elif x >= arr.GetLength 0 then loop 0 (y + 1) acc
        elif arr[x, y] = '#' then loop (x + 1) y ((x, y) :: acc)
        else loop (x + 1) y acc

    loop 0 0 [] |> Array.ofList

let transform input =
    let arr = input |> Seq.map Seq.toArray |> Seq.toArray
    let width = arr[0].Length
    let height = arr.Length

    Array2D.init width height (fun x y -> arr[y][x])

let findNonEmptyLines f arr = arr |> Seq.map f |> Set.ofSeq

let adjustedNonEmptyLines z1 z2 nonEmptyLines =
    nonEmptyLines
    |> Seq.filter (fun z -> z > min z1 z2 && z <= max z1 z2)
    |> Seq.length

let distanceAll nonEmptyRows nonEmptyColumns pairs =
    let distance ((x1, y1), (x2, y2)) =
        let dx = abs (x1 - x2)
        let dy = abs (y1 - y2)
        let emptyX = dx - (nonEmptyColumns |> adjustedNonEmptyLines x1 x2)
        let emptyY = dy - (nonEmptyRows |> adjustedNonEmptyLines y1 y2)
        dx + dy + emptyX + emptyY

    pairs |> Seq.map distance

let run input =
    let arr = input |> transform
    let galaxies = arr |> findGalaxies
    let nonEmptyRows = galaxies |> findNonEmptyLines snd
    let nonEmptyColumns = galaxies |> findNonEmptyLines fst

    galaxies |> uniquePairs |> distanceAll nonEmptyRows nonEmptyColumns

let solve = Utils.FileReading.readLines >> run >> Seq.sum

"input.txt" |> solve |> printfn "%A"
