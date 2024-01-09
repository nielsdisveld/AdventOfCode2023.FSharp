module Part1

let handValue (hand: string) =
    let handType = hand |> Seq.countBy id |> Seq.map snd
    let highest = handType |> Seq.max
    let distinct = handType |> Seq.length
    let getValue = Common.getValue 11
    (highest, -distinct, getValue 0 hand, getValue 1 hand, getValue 2 hand, getValue 3 hand, getValue 4 hand)
