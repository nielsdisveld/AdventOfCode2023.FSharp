module Common

let numbers (str: string) =
    str.Split ' ' |> Array.filter (fun s -> s.Length > 0) |> Array.map int

let parseLine (str: string) =
    match str.Split [| ':'; '|' |] with
    | [| _; winning; player |] -> (numbers winning, numbers player)
    | _ -> failwith $"Invalid line %s{str}"

let correct (winning, player) =
    player |> Array.filter (fun n -> Array.contains n winning) |> Array.length
