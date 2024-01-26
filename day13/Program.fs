let splitInput input =
    let folder acc line =
        match line, acc with
        | "", _ -> [] :: acc
        | _, h :: tail -> (line :: h) :: tail
        | _ -> failwith "Invalid input."

    input |> Seq.rev |> Seq.fold folder [ [] ]

let diffs arr1 (arr2: _[,]) =
    arr1
    |> Array2D.mapi (fun x y v -> if arr2[x, y] = v then 0 else 1)
    |> Utils.Array2D.sum

let pattern input =
    let arr = input |> Seq.map Seq.toArray |> Seq.toArray

    Array2D.init arr[0].Length arr.Length (fun x y ->
        match arr[y][x] with
        | '#' -> true
        | '.' -> false
        | _ -> failwith "Invalid char.")

let flip (arr: _[,]) =
    let l = (arr.GetLength 0) - 1
    Array2D.init (arr.GetLength 0) (arr.GetLength 1) (fun x y -> arr[l - x, y])

let transpose (arr: _[,]) =
    Array2D.init (arr.GetLength 1) (arr.GetLength 0) (fun x y -> arr[y, x])

let hasMirrorAt comparer (arr: _[,]) (flipped: _[,]) x =
    let width = arr.GetLength 0
    let range = min x (width - x) - 1
    let xflip = width - x
    let arr1 = arr[x .. (x + range), 0..]
    let arr2 = flipped[xflip .. (xflip + range), 0..]
    comparer arr1 arr2

let findMirror comparer arr =
    let flipped = flip arr
    let width = Array2D.length1 arr - 1

    Seq.init width (fun i -> i + 1)
    |> Seq.tryFind (hasMirrorAt comparer arr flipped)
    |> Option.defaultValue 0

let findMirror2 comparer arr =
    (findMirror comparer arr, arr |> transpose |> (findMirror comparer))

let comparer1 arr1 arr2 = arr1 = arr2
let comparer2 arr1 arr2 = diffs arr1 arr2 = 1
let score (a, b) = a + 100 * b

let solve comparer =
    Utils.FileReading.readLines
    >> splitInput
    >> List.map pattern
    >> List.map (findMirror2 comparer)
    >> List.sumBy score

solve comparer1 "input.txt" |> printfn "%A"
solve comparer2 "input.txt" |> printfn "%A"
