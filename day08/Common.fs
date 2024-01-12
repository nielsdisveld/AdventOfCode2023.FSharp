module Common

type Nodes = Map<string, string * string>

let parseNode (str: string) = str[0..2], (str[7..9], str[12..14])

let parseInput (sq: string seq) =
    let instructions = sq |> Seq.head
    let nodes = sq |> Seq.skip 2 |> Seq.map parseNode |> Map.ofSeq
    (instructions, nodes)

let navigate (left, right) (c: char) =
    match c with
    | 'L' -> left
    | 'R' -> right
    | _ -> failwith $"Incorrect instruction: %c{c}"

let navigateMany (nodes: Nodes) (instructions: string) (start: string) =
    instructions
    |> Seq.fold (fun current instruction -> navigate (nodes.Item current) instruction) start
