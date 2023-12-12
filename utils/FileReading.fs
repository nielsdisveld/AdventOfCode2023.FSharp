namespace Utils

open System.IO

module FileReading =
    let readLines (filename: string) =
        seq {
            let reader = new StreamReader(filename)

            while not reader.EndOfStream do
                yield reader.ReadLine()
        }
