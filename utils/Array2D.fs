namespace Utils

module Array2D =

    let findIndex (value: 'T) (arr: 'T[,]) =
        let rec loop x y =
            if y >= arr.GetLength 1 then None
            elif x >= arr.GetLength 0 then loop 0 (y + 1)
            elif arr[x, y] = value then Some(x, y)
            else loop (x + 1) y

        loop 0 0

    let getByTuple (x, y) (arr: 'T[,]) = arr[x, y]
