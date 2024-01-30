namespace Utils

module Array2D =

    let findIndex (value: 'T) (arr: 'T[,]) =
        let rec loop x y =
            if y >= arr.GetLength 1 then None
            elif x >= arr.GetLength 0 then loop 0 (y + 1)
            elif arr[x, y] = value then Some(x, y)
            else loop (x + 1) y

        loop 0 0

    let sum (arr: _[,]) =
        let rec loop x y acc =
            if y = arr.GetLength 1 then acc
            elif x = arr.GetLength 0 then loop 0 (y + 1) acc
            else loop (x + 1) y (acc + arr[x, y])

        loop 0 0 0

    let print s (arr: _[,]) =
        let heigth = arr.GetLength 1

        for i in 0 .. heigth - 1 do
            arr[0.., i]
            |> Seq.map (fun x -> x.ToString())
            |> Seq.reduce (fun s1 s2 -> s1 + s + s2)
            |> printfn "%A"
