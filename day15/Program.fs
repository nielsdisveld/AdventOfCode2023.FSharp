type Cmd =
    | Add of int
    | Remove

let parseLine (str: string) = str.Split ','

let hashCode (str: string) =
    let folder acc c =
        acc |> (fun a -> a + int c) |> (fun a -> 17 * a) |> (fun a -> a % 256)

    str |> Seq.fold folder 0

let parseStep (str: string) =
    match str.Split [| '='; '-' |] with
    | [| label; "" |] -> label, Remove
    | [| label; focal |] -> label, Add(int focal[0] - int '0')
    | _ -> failwith "Invalid step."

let removeFromBox box label =
    box |> Array.filter (fun (l, _) -> l <> label)

let addToBox box (label, focal) =
    let iopt = box |> Array.tryFindIndex (fun (l, _) -> l = label)

    match iopt with
    | Some i -> box |> Array.updateAt i (label, focal)
    | None -> Array.append box [| (label, focal) |]

let scoreArray (i, arr) =
    arr |> Seq.mapi (fun j (_, focal) -> (i + 1) * (j + 1) * focal) |> Seq.sum

let scoreMap map =
    map |> Map.toSeq |> Seq.sumBy scoreArray

let run cmds =
    let map = Seq.init 256 (fun i -> (i, [||])) |> Map.ofSeq

    let folder (map: Map<int, _>) (label, cmd) =
        let i = hashCode label
        let box = map.Item i

        match cmd with
        | Add focal ->
            let updatedBox = (label, focal) |> addToBox box
            map |> Map.add i updatedBox
        | Remove ->
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
