namespace Utils

module Math =
    let divPositive m n = ((m % n) + n) % n
    let rec gcd x y = if y = 0 then x else gcd y (x % y)
    let rec gcdMany (sq: int seq) = sq |> Seq.reduce gcd

    let extEuclidean (n1: int64) (m1: int64) =
        let rec stepper acc n m =
            let r = n % m
            let d = n / m

            if r = 0L then
                (d, m, 0L) :: acc
            else
                let equation = (d, m, r) // Equivalent to: n = d*m + r
                stepper (equation :: acc) m r

        let folder (a, b, d) (* Equivalent to: a*n+b*m=d *) (k, m, r) (*// Equivalent to: n = k*m + r *) =
            if r = 0L then (0L, 1L, m) else (b, a - b * k, d)

        let (a, b, d) = stepper [] (max n1 m1) (min n1 m1) |> List.fold folder (0L, 0L, 0L)
        if (n1 >= m1) then (a, b, d) else (b, a, d)

    let chineseRemainder (k1, n1) (k2, n2) =
        let (a, c, d) = extEuclidean n1 n2

        if k1 % d <> k2 % d then
            failwith $"No possible solution for (%A{(k1, n1)}) and %A{(k2, n2)})"

        let solMod = (n1 * n2) / d
        let solution = (k1 * c * n2 + k2 * a * n1) / d
        divPositive solution solMod, solMod
