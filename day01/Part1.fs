module Part1

let toDigit c = int c - int '0'
let isDigit c = c >= 0 && c <= 9
let fstVal s = Seq.find (fun _ -> true) s
let sndVal s = Seq.findBack (fun _ -> true) s
let calibrationVal s = (10 * fstVal s) + sndVal s

let solve =
    Utils.FileReading.readLines
    >> Seq.map (Seq.map toDigit)
    >> Seq.map (Seq.filter isDigit)
    >> Seq.map calibrationVal
    >> Seq.reduce (+)
