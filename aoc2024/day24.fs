module aoc2024.day24

open System.Collections.Generic
open aoc2024.util

(*
x00: 1
x01: 1
x02: 1
y00: 0
y01: 1
y02: 0

x00 AND y00 -> z00
x01 XOR y01 -> z01
x02 OR y02 -> z02
*)

type Operation =
    | AND of string * string
    | XOR of string * string
    | OR of string * string

type Instruction = Operation * string

let solve () =
    let io = aocIO
    let inp = io.getInput ()

    let wires = Dictionary<string, int option>()
    let mutable instructions = List.empty

    let mutable processingWires = true

    let gv (w: int option) =
        match w with
        | None -> -1
        | Some(v) -> v

    let gcw w =
        if not (wires.ContainsKey(w)) then
            wires[w] <- None

        wires[w]

    let mutable z = []

    let mutable carries = []
    let mutable zsetters = []
    let mutable sums = []

    for line in inp do
        if line.Length = 0 then
            processingWires <- false
        else if (processingWires) then
            let spl = line.Split ":"
            wires[spl[0]] <- spl[1] |> int |> option.Some
        else
            let spl = line.Split " "


            let op =
                match spl[1] with
                | "AND" -> AND(spl[0], spl[2])
                | "OR" -> OR(spl[0], spl[2])
                | "XOR" -> XOR(spl[0], spl[2])

            if (spl[4].StartsWith "z") then
                if not (z |> List.contains (spl[4])) then
                    z <- z @ [ spl[4] ]

            let out = spl[4]

            match op with
            | AND(x, y) ->
                if ((x.StartsWith "x" && y.StartsWith "y")) then
                    carries <- carries @ [ (x, y, out) ]

                if ((x.StartsWith "y" && y.StartsWith "x")) then
                    carries <- carries @ [ (y, x, out) ]
            | XOR(x, y) ->
                if ((x.StartsWith "x" && y.StartsWith "y")) then
                    sums <- sums @ [ (x, y, out) ]

                if ((x.StartsWith "y" && y.StartsWith "x")) then
                    sums <- sums @ [ (y, x, out) ]

            | _ -> ()

            if (out.StartsWith "z") then
                zsetters <- zsetters @ [ (spl[0], spl[2], out, spl[1]) ]

            instructions <- instructions @ [ Instruction(op, out) ]

    (*
    for (x, y, z) in carries |> List.sortBy (fun (x, y, z) -> x) do
        printfn $"{z}"

    printfn $"break"
    *)

    for (x, y, z) in sums |> List.sortBy (fun (x, y, z) -> x) do
        printfn $"{z}"

    for (x, y, z, op) in zsetters |> List.sortBy (fun (x, y, z, op) -> z) do
        printfn $"{x} {op} {y} {z}"

    z <- z |> List.sort

    for x in 0..44 do
        let mutable i = 0

        for y in wires.Keys |> Seq.where (fun z -> z.StartsWith "x" || z.StartsWith "y") do
            if (i = 9) then wires[y] <- Some(0) else wires[y] <- Some(0)


    let printZ () =
        let mutable n = ""

        for zw in z do
            if (wires.ContainsKey(zw)) then
                let v = wires[zw].Value.ToString()
                n <- v + n
            else
                n <- "0" + n

        n <- "0b" + n
        printfn $"%A{n}"
        printfn $"%A{uint64 n}"

    let setBy = Dictionary<string, string list>()

    let gcs x y =
        if not (setBy.ContainsKey(x)) then
            setBy[x] <- []

        setBy[x] <- setBy[x] @ [ y ]

    while (true) do
        let previousZ =
            z |> Seq.where (wires.ContainsKey) |> Seq.map (fun x -> wires[x]) |> Seq.toList

        for op, out in instructions do
            match op with
            | AND(w1, w2) ->
                let o1, o2 = w1, w2
                let w1 = gv (gcw w1)
                let w2 = gv (gcw w2)

                if (w1 = 1 && w2 = 1) then
                    gcs out o1
                    gcs out o2
                    wires[out] <- Some(1)
                else if (w1 = 0 || w2 = 0) then
                    gcs out o1
                    gcs out o2
                    wires[out] <- Some(0)
            | OR(w1, w2) ->
                let o1, o2 = w1, w2
                let w1 = gv (gcw w1)
                let w2 = gv (gcw w2)

                if (w1 = 1 || w2 = 1) then
                    gcs out o1
                    gcs out o2
                    wires[out] <- Some(1)
                else if (w1 = 0 && w2 = 0) then
                    gcs out o1
                    gcs out o2
                    wires[out] <- Some(0)
            | XOR(w1, w2) ->
                let o1, o2 = w1, w2
                let w1 = gv (gcw w1)
                let w2 = gv (gcw w2)

                if (w1 <> w2) then
                    gcs out o1
                    gcs out o2
                    wires[out] <- Some(1)
                else
                    gcs out o1
                    gcs out o2
                    wires[out] <- Some(0)

        let zNow =
            z |> Seq.where (wires.ContainsKey) |> Seq.map (fun x -> wires[x]) |> Seq.toList


        if (previousZ = zNow) then
            printZ ()

            let rec findAll (x: string) (acc: HashSet<string>) (visited: string list) (depth: int) =
                if depth > 5 || visited |> List.contains x then
                    acc
                else if not (setBy.ContainsKey(x)) then
                    acc.Add(x) |> ignore
                    acc
                else
                    acc.Add(x)
                    let visited = visited @ [ x ]
                    setBy[x] |> List.map (fun y -> findAll y acc visited (depth + 1)) |> ignore

                    acc

            (*
            for w in wires do
                let mutable wname = w.Key

                while (setBy |> Seq.tryFind (fun x -> x.Value |> List.contains wname) |> Option.isSome) do
                    let origin = (setBy |> Seq.find (fun x -> x.Value |> List.contains wname)).Key
                    wname <- origin

                let mutable wrongOnes = []

                if ((w.Key.StartsWith "x" || w.Key.StartsWith "y") && wname.StartsWith "z") then
                    if (w.Key[1..] <> wname[1..]) then
                        wrongOnes <- wrongOnes @ [ wname ]

                for w in wrongOnes do
                    let setb = findAll w (HashSet()) [] 0
                    printf $"Wrong %s{w}: "

                    for x in setb do
                        printf $"%s{x}, "

                    printfn $""
                    *)


            for x in wires do
                if (x.Value.IsNone) then
                    printfn $"%A{x}"

            exit (0)



            //printfn $"%A{(w.Key, wname)}"

            exit (0)

    printZ ()
    exit (0)

    0
