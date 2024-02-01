type Cmd =
    | Add of string * int
    | Remove of string

let parseLine (str: string) = str.Split ','

let hashCode (str: string) =
    let folder acc c =
        acc |> (fun a -> a + int c) |> (fun a -> 17 * a) |> (fun a -> a % 256)

    str |> Seq.fold folder 0

let parseStep (str: string) =
    match str.Split [| '='; '-' |] with
    | [| label; "" |] -> Remove label
    | [| label; focal |] -> Add(label, int focal[0] - int '0')
    | _ -> failwith "Invalid step."

let removeFromBox box label =
    box |> Array.filter (fun (l, _) -> l <> label)

let addToBox box (label, focal) =
    let iopt = box |> Array.tryFindIndex (fun (l, _) -> l = label)

    match iopt with
    | Some i -> box |> Array.updateAt i (label, focal)
    | None -> Array.append box [| (label, focal) |]

let scoreArray (i, arr) =
    arr |> Array.mapi (fun j (_, focal) -> (i + 1) * (j + 1) * focal)

let scoreMap map =
    map |> Map.toSeq |> Seq.map scoreArray |> Seq.sumBy Array.sum

let run cmds =
    let map = Seq.init 256 (fun i -> (i, Array.empty)) |> Map.ofSeq

    let folder (map: Map<int, _[]>) cmd =
        match cmd with
        | Add(label, focal) ->
            let i = hashCode label
            let box = map.Item i
            let updatedBox = (label, focal) |> addToBox box
            map |> Map.add i updatedBox
        | Remove label ->
            let i = hashCode label
            let box = map.Item i
            let updatedBox = label |> removeFromBox box
            map |> Map.add i updatedBox

    cmds |> Seq.fold folder map

let part1 =
    Utils.FileReading.readLines
    >> Seq.head
    >> parseLine
    >> Array.map hashCode
    >> Array.sum

let part2 =
    Utils.FileReading.readLines
    >> Seq.head
    >> parseLine
    >> Seq.map parseStep
    >> run
    >> scoreMap

part1 "input.txt" |> printfn "%A"
part2 "input.txt" |> printfn "%A"
