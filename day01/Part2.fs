module Part2

let substrings =
    [| ("one", 1)
       ("1", 1)
       ("two", 2)
       ("2", 2)
       ("three", 3)
       ("3", 3)
       ("four", 4)
       ("4", 4)
       ("five", 5)
       ("5", 5)
       ("six", 6)
       ("6", 6)
       ("seven", 7)
       ("7", 7)
       ("eight", 8)
       ("8", 8)
       ("nine", 9)
       ("9", 9) |]

let digit findSubstr minOrMaxBy =
    substrings
    |> Array.map (fun (ss, v) -> findSubstr ss, v)
    |> Array.filter (fun (i, _) -> i > -1)
    |> minOrMaxBy fst
    |> snd

let fstDigit (str: string) = digit (str.IndexOf) Array.minBy

let sndDigit (str: string) = digit (str.LastIndexOf) Array.maxBy

let calibrationValues (str: string) = 10 * fstDigit str + sndDigit str

let solve =
    Utils.FileReading.readLines
    >> Seq.map Seq.toArray
    >> Seq.map (fun s -> System.String s)
    >> Seq.map calibrationValues
    >> Seq.reduce (+)
