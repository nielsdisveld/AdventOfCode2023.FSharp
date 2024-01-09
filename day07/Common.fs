module Common

let parseLine (str: string) =
    match str.Split ' ' with
    | [| hand; bid |] -> (hand, int bid)
    | _ -> failwith $"%s{str}"

let cardToValue jVal =
    function
    | 'A' -> 14
    | 'K' -> 13
    | 'Q' -> 12
    | 'J' -> jVal
    | 'T' -> 10
    | c -> int c - int '0'

let getValue jVal i (str: string) = str[i] |> cardToValue jVal

let run handValue (hands: (string * int) seq) =
    hands
    |> Seq.sortBy (fst >> handValue)
    |> Seq.mapi (fun i (_, bid) -> (1 + int i) * bid)
    |> Seq.sum

let solve handValue =
    Utils.FileReading.readLines >> Seq.map parseLine >> run handValue
