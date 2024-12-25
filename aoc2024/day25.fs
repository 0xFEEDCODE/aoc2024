module aoc2024.day25

open aoc2024.util

(*
#####
.####
.####
.####
.#.#.
.#...
.....
*)

let solve () =
    let io = aocIO
    let inp = io.getInput ()

    let mutable schematics = []

    let mutable sb = []

    for line in inp do
        if (line |> Seq.isEmpty) then
            schematics <- schematics @ [ sb ]
            sb <- []
        else
            sb <- sb @ [ line ]

    schematics <- schematics @ [ sb ]

    let schematics = schematics |> List.map (Grid.initializeFromStringSeq)

    let keys = schematics |> Seq.where (fun x -> x[0][0] <> '#')
    let locks = schematics |> Seq.where (fun x -> x[0][0] = '#')

    let keyPinHeights =
        keys
        |> Seq.map (fun key ->
            let mutable pinHeights = []

            for x in 0 .. key[0].Length - 1 do
                let mutable found = false

                for y in 1 .. key.Length - 1 do
                    if not found then
                        if (key[y][x] <> key[y - 1][x]) then
                            pinHeights <- pinHeights @ [ (key.Length - y - 1) ]
                            found <- true

            pinHeights)

    let lockPinHeights =
        locks
        |> Seq.map (fun key ->
            let mutable pinHeights = []

            for x in 0 .. key[0].Length - 1 do
                let mutable found = false

                for y in 1 .. key.Length - 1 do
                    if not found then
                        if (key[y][x] <> key[y - 1][x]) then
                            pinHeights <- pinHeights @ [ (y - 1) ]
                            found <- true

            pinHeights)


    (*printfn $"Pins"

    for lp in lockPinHeights do
        printfn $"%A{lp}"

    printfn $"Keys"

    for kp in keyPinHeights do
        printfn $"%A{kp}"*)


    let mutable canFit = 0
    let rows = locks |> Seq.head |> Seq.length
    let cols = (locks |> Seq.head)[0] |> Seq.length

    for lp in lockPinHeights do
        for kp in keyPinHeights do
            let isFit =
                [ for i in 0..cols-1 do
                      i ]
                |> Seq.forall (fun i -> (lp[i] + kp[i]) < rows - 1)

            if isFit then
                canFit <- canFit + 1
    printfn $"%A{canFit}"



    0
