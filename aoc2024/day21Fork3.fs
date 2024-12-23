module aoc2024.daywqe

open System
open System.Collections.Generic
open System.Text
open Microsoft.FSharp.Core
open aoc2024.util


let tryGetValue k (d: Dictionary<_, _>) =
    match d.TryGetValue k with
    | true, v -> Some v
    | _ -> None

type pad =
    | NPAD
    | DPAD

let solve () =
    let io = aocIO
    let inp = io.getInput ()
    let codes = inp

    let npad =
        [| [| '7'; '8'; '9' |]
           [| '4'; '5'; '6' |]
           [| '1'; '2'; '3' |]
           [| 'X'; '0'; 'A' |] |]

    let npadGetV (p: Point2D) = (npad[p.y][p.x])

    let mutable npadButtonPositions = Dictionary<char, Point2D>()

    for y in 0 .. npad.Length - 1 do
        for x in 0 .. (npad[0].Length) - 1 do
            let p = Point2D(x, y)
            npadButtonPositions.Add(npadGetV p, p)

    let dpad = [| [| 'X'; '^'; 'A' |]; [| '<'; 'v'; '>' |] |]
    let dpadGetV (p: Point2D) = (dpad[p.y][p.x])

    let mutable dpadButtonPositions = Dictionary<char, Point2D>()

    for y in 0 .. dpad.Length - 1 do
        for x in 0 .. dpad[0].Length - 1 do
            let p = Point2D(x, y)
            dpadButtonPositions.Add(dpadGetV p, p)

    let npadSp = Point2D(2, 3)
    let dpadSp = Point2D(2, 0)

    let offsets =
        [ ('<', (-1, 0)); ('>', (1, 0)); ('^', (0, -1)); ('v', (0, 1)) ]
        |> List.map (fun x -> (fst x, snd x |> Point2D))

    let offsetMap =
        Map.empty
            .Add('<', Point2D(-1, 0))
            .Add('>', Point2D(1, 0))
            .Add('^', Point2D(0, -1))
            .Add('v', Point2D(0, 1))

    let shortestPathsToPressButton (button: char) startPos boundX boundY fn =
        let isPointWithinBoundaries (p: Point2D) =
            (p.y >= 0 && p.y < boundY && p.x >= 0 && p.x < boundX)

        let q = PriorityQueue()
        q.Enqueue((startPos, ""), 0)

        let cons = HashSet()

        let mutable paths = []
        let mutable shortestPathFound = None

        while (q.Count > 0) do
            let cp, cpath = q.Dequeue()

            if ((fn cp) = button) then
                if (shortestPathFound.IsNone || shortestPathFound.Value > cpath.Length) then
                    shortestPathFound <- Some(cpath.Length)

                paths <- paths @ [ cpath ]
            else if (shortestPathFound.IsNone || cpath.Length < shortestPathFound.Value) then
                offsets
                |> Seq.where (fun (_, offs) -> isPointWithinBoundaries (offs + cp) && fn (offs + cp) <> 'X')
                |> Seq.iter (fun (ch, offs) ->
                    let nextPoint = offs + cp
                    let newPath = cpath + ch.ToString()

                    if
                        not (cons.Contains(newPath))
                        && (shortestPathFound.IsNone || cpath.Length <= shortestPathFound.Value)
                    then
                        cons.Add(newPath) |> ignore
                        let newPath = cpath + ch.ToString()
                        q.Enqueue((nextPoint, newPath), cpath.Length))

        paths

    let getPathsForSeq (dseq: string) (pad: pad) =
        let startPos =
            match pad with
            | NPAD -> npadSp
            | DPAD -> dpadSp

        let q = PriorityQueue()
        q.Enqueue((startPos, "", [], dseq), 0)

        let mutable paths = List()
        let mutable shortestPath = None

        while (q.Count > 0) do
            let cp, cpath, pathSlices, dpadSeq = q.Dequeue()

            if (dpadSeq.Length = 0) then
                if (shortestPath.IsNone || shortestPath.Value > cpath.Length) then
                    shortestPath <- Some(cpath.Length)

                paths.Add((cpath, pathSlices))
            else if (shortestPath.IsNone || cpath.Length <= shortestPath.Value) then
                let target = dpadSeq[0]

                let targetPos =
                    match pad with
                    | NPAD -> npadButtonPositions[target]
                    | DPAD -> dpadButtonPositions[target]

                let paths =
                    match pad with
                    | DPAD -> shortestPathsToPressButton target cp (dpad[0].Length) dpad.Length dpadGetV
                    | NPAD -> shortestPathsToPressButton target cp (npad[0].Length) npad.Length npadGetV

                paths
                |> Seq.iter (fun p ->
                    let newPath = cpath + p + "A"
                    q.Enqueue((targetPos, newPath, pathSlices @ [ [ p ] ], dpadSeq[1..]), cpath.Length))

        paths

    let aCount (x: string) =
        x |> Seq.where (fun y -> y = 'A') |> Seq.length

    let isWithinBoundariesNpad (p: Point2D) =
        (p.y >= 0 && p.y < npad.Length && p.x >= 0 && p.x < npad[0].Length)
        && (npad[p.y][p.x] <> 'X')

    let isWithinBoundariesDpad (p: Point2D) =
        (p.y >= 0 && p.y < dpad.Length && p.x >= 0 && p.x < dpad[0].Length)
        && (dpad[p.y][p.x] <> 'X')

    let getPossibleCombinations () =
        dpadButtonPositions.Values
        |> Seq.where (fun p -> dpad[p.y][p.x] <> 'X')
        |> Seq.map (fun x ->
            dpadButtonPositions.Keys
            |> Seq.where (fun x -> x <> 'X')
            |> Seq.map (fun button -> shortestPathsToPressButton button x (dpad[0].Length) dpad.Length dpadGetV)
            |> Seq.collect id)
        |> Seq.collect id

    let processFinal (buttonPresses: string) =
        let mutable resSb = StringBuilder()
        let mutable cp = npadSp

        let mutable i = 0
        let len = buttonPresses |> Seq.length

        while (i < len && isWithinBoundariesNpad cp) do
            let bp = buttonPresses[i]

            if (bp <> 'A') then
                let offs = offsetMap[bp]
                cp <- cp + offs
            else
                resSb.Append(npad[cp.y][cp.x]) |> ignore

            i <- i + 1

        let res = resSb.ToString()

        if
            (res.Length = 4
             && res[0] <> res[1]
             && res[1] <> res[2]
             && res[2] <> res[3]
             && res[3] = 'A'
             && (aCount res) = 1)
        then
            Some(res)
        else
            None


    let proc (buttonPresses: string) nLoops =
        let mutable buttonPresses = buttonPresses

        let mutable loopsLeft = nLoops - 1

        let pathSb = StringBuilder()

        while (loopsLeft > 0 && buttonPresses.Length > 0 && aCount buttonPresses >= 4) do

            let mutable cp = dpadSp
            pathSb.Clear() |> ignore

            let mutable i = 0
            let len = buttonPresses |> Seq.length

            while (i < len && isWithinBoundariesDpad cp) do
                let bp = buttonPresses[i]

                if (bp <> 'A') then
                    let offs = offsetMap[bp]
                    cp <- cp + offs
                else
                    pathSb.Append(dpad[cp.y][cp.x]) |> ignore

                i <- i + 1


            buttonPresses <- pathSb.ToString()

            loopsLeft <- loopsLeft - 1

        if loopsLeft = 0 && buttonPresses.Length > 0 then
            //printfn $"Last loop buttons: %A{buttonPresses}"
            processFinal buttonPresses
        else
            None

    let procCheck (buttonPresses: string) =
        let mutable cp = dpadSp

        let mutable i = 0
        let len = buttonPresses |> Seq.length

        while (i < len && isWithinBoundariesDpad cp) do
            let bp = buttonPresses[i]

            if (bp <> 'A') then
                let offs = offsetMap[bp]
                cp <- cp + offs

            i <- i + 1

        let ac = aCount buttonPresses
        i >= len && ac > 4 && ac <> len && buttonPresses[0] <> 'A'

    let procCheck2 (buttonPresses: string) (target: string) =
        let mutable buttonPresses = buttonPresses

        let pathSb = StringBuilder()

        let mutable cp = dpadSp
        pathSb.Clear() |> ignore

        let mutable i = 0
        let len = buttonPresses |> Seq.length

        while (i < len && isWithinBoundariesDpad cp) do
            let bp = buttonPresses[i]

            if (bp <> 'A') then
                let offs = offsetMap[bp]
                cp <- cp + offs
            else
                pathSb.Append(dpad[cp.y][cp.x]) |> ignore

            i <- i + 1

        target = pathSb.ToString()

    let combinations = getPossibleCombinations () |> Seq.toArray

    let example = "<v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A"

    for code in codes do
        let pathsNpad = getPathsForSeq code NPAD

        let pathsDpad =
            pathsNpad |> Seq.map (fun x -> getPathsForSeq (fst x) DPAD) |> Seq.collect id

        let combinations =
            (pathsDpad |> Seq.map snd |> Seq.collect id)
            |> Seq.collect id
            |> Seq.toArray
            |> Array.distinct

        printfn $"%A{combinations.Length}"

        let cons = HashSet()

        let r = proc example 3
        printfn $"%A{(aCount example, example.Length)}"
        printfn $"%A{r}"


        (*
                let combinations =
                    combinations
                    |> Array.where (fun x ->
                        let mutable np = cp

                        x
                        |> Seq.forall (fun ch ->
                            np <- np + offsetMap[ch]
                            isWithinBoundariesDpad np))
                            *)

        while true do
            let mutable path = ""
            let mutable res = None
            let mutable cp = dpadSp

            let mutable tries = 0

            while (path.Length < 28 && res.IsNone) do
                if (tries >= 10) then
                    path <- ""
                    cp <- dpadSp
                    tries <- 0

                let r = Random()

                let random = r.Next(0, combinations |> Seq.length)
                let nextPart = combinations[random]

                let np =
                    let mutable np = cp

                    for ch in nextPart do
                        np <- np + offsetMap[ch]

                    np

                if (isWithinBoundariesDpad np) then
                    let n = 50

                    let npath = path + nextPart + "A"

                    if (path.Length < n || (path.Length >= n && procCheck npath)) then
                        path <- npath
                        cp <- np
                        tries <- 0
                    else
                        tries <- tries + 1


            let fp = path.ToString()
            let r = proc fp 2


            if (r.IsSome && r.Value = code) then
                exit (0)
    (*
            let fp = sb.ToString()
            printfn $"start"
            let r = proc fp 3
            printfn $"%A{(r, fp)}"
            let r = proc example 3
            printfn $"%A{(r, example)}"
            exit (0)
            *)

    (*
            let fp = path.ToString()
            let r = proc fp 2

            if r.IsSome then
                printfn $"Found!"

                if (r.Value = "029A") then
                    exit 0

                printfn $"%A{(r.Value, fp)}"
                *)



    let r = proc example 3
    printfn $"%A{r}"
    printfn $"%A{combinations}"
