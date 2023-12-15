module Common

type Part = (int * int) * string // Starting position and digit string

let symbols = [| '#'; '$'; '%'; '&'; '*'; '+'; '-'; '/'; '='; '@' |]

let nonDigits = Array.append symbols [| '.' |]

let addBlankRows (rows: seq<string>) =
    let blankRow = Seq.head rows |> String.map (fun _ -> '.') |> Seq.singleton
    Seq.append (Seq.append blankRow rows) blankRow

let addStartIndices (numStrings: string[]) =
    let folder i (numStr: string) = i + numStr.Length + 1

    Array.scan folder 0 numStrings
    |> Array.removeAt numStrings.Length
    |> Array.zip numStrings


let hasSymbolAt (symbolStr: string) startIndex endIndex =
    symbolStr[max 0 startIndex .. min endIndex symbolStr.Length]
    |> String.exists (fun c -> Array.contains c symbols)

let hasSymbol (symbolStr: string) (numStr: string, startIndex) =
    hasSymbolAt symbolStr (startIndex - 1) (startIndex + numStr.Length)

let isPart (prevRow, currRow, nextRow) (numStr: string, startIndex) =
    numStr.Length > 0
    && (hasSymbol prevRow (numStr, startIndex)
        || hasSymbol currRow (numStr, startIndex)
        || hasSymbol nextRow (numStr, startIndex))

let getConnectedToSymbol j (adjRows: string[]) : Part[] =
    let (prevRow, currRow, nextRow) = (adjRows[0], adjRows[1], adjRows[2])
    let numStrings = currRow.Split nonDigits

    addStartIndices numStrings
    |> Array.filter (isPart (prevRow, currRow, nextRow))
    |> Array.map (fun (numStr, i) -> (i, j), numStr)

let getParts (rows: seq<string>) : seq<Part> =
    addBlankRows rows
    |> Seq.windowed 3
    |> Seq.mapi getConnectedToSymbol
    |> Seq.collect id
