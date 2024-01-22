let pairs sq =
    sq
    |> Seq.allPairs sq
    |> Seq.filter (fun (a, b) -> a <> b) // Remove pairs of points with themselves
    |> Seq.distinctBy (fun (a, b) -> min a b, max a b) // Remove duplicate pairs

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

let nonEmptyBetween z1 z2 nonEmptyLines =
    nonEmptyLines
    |> Seq.filter (fun z -> z > min z1 z2 && z <= max z1 z2)
    |> Seq.length

let distance i nonEmptyRows nonEmptyColumns ((x1, y1), (x2, y2)) =
    let dx = abs (x1 - x2)
    let dy = abs (y1 - y2)
    let expandedX = dx - (nonEmptyColumns |> nonEmptyBetween x1 x2)
    let expandedY = dy - (nonEmptyRows |> nonEmptyBetween y1 y2)
    let totalExt = (i - 1L) * int64 (expandedX + expandedY)
    int64 (dx + dy) + totalExt

let run i input =
    let arr = input |> transform
    let galaxies = arr |> findGalaxies
    let nonEmptyRows = galaxies |> findNonEmptyLines snd
    let nonEmptyColumns = galaxies |> findNonEmptyLines fst

    galaxies |> pairs |> Seq.map (distance i nonEmptyRows nonEmptyColumns)

let solve i =
    Utils.FileReading.readLines >> run i >> Seq.sum

"input.txt" |> solve 2L |> printfn "%A" // Part1
"input.txt" |> solve 1000000L |> printfn "%A" // Part2
