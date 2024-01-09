module Seq

let tryMax (sq: 'T seq) =
    match sq |> Seq.length with
    | 0 -> None
    | _ -> sq |> Seq.max |> Some
