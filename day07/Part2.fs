module Part2

let handValue (hand: string) =
    let js = hand |> String.filter (fun c -> c = 'J') |> String.length

    let handType =
        hand |> String.filter (fun c -> c <> 'J') |> Seq.countBy id |> Seq.map snd

    let highest = handType |> Seq.tryMax |> Option.defaultValue 0
    let distinct = max (handType |> Seq.length) 1
    let getValue = Common.getValue 1
    (highest + js, -distinct, getValue 0 hand, getValue 1 hand, getValue 2 hand, getValue 3 hand, getValue 4 hand)
