module Part1

let solve =
    Utils.FileReading.readLines
    >> Common.getParts
    >> Seq.map (snd >> int)
    >> Seq.sum
