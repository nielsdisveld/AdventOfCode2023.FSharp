module Part1

let points c = if c = 0 then 0 else pown 2 (c - 1)

let solve =
    Utils.FileReading.readLines
    >> Seq.map Common.parseLine
    >> Seq.sumBy (Common.correct >> points)
