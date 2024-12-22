module aoc2024.XZ

open System
open System.Collections.Generic
open aoc2024.util


let tryGetValue k (d: Dictionary<_, _>) =
    match d.TryGetValue k with
    | true, v -> Some v
    | _ -> None

(*
    let getBestPathForSequence (seq: string) (pad: pad) =
        let startPos =
            match pad with
            | NPAD -> npadSp
            | DPAD -> dpadSp

        let q = Queue()
        q.Enqueue((startPos, "", seq))

        let mutable paths = List()
        let mutable shortestPathFound = None

        while (q.Count > 0) do
            let cp, cpath, seq = q.Dequeue()

            if (seq.Length = 0) then
                if (shortestPathFound.IsNone || cpath.Length < shortestPathFound.Value) then
                    shortestPathFound <- Some(cpath.Length)

                paths.Add(cpath)
            else if (shortestPathFound.IsNone || cpath.Length <= shortestPathFound.Value) then
                let target = seq[0]

                let targetPos =
                    match pad with
                    | NPAD -> npadButtonPositions[target]
                    | DPAD -> dpadButtonPositions[target]

                let paths =
                    match pad with
                    | DPAD -> shortestPathsToPressButton target cp (dpad[0].Length) (dpad.Length) dpadGetV
                    | NPAD -> shortestPathsToPressButton target cp (npad[0].Length) (npad.Length) npadGetV

                paths
                |> Seq.iter (fun p ->
                    let newPath = cpath + p + "A"
                    q.Enqueue((targetPos, newPath, seq[1..])))

        let mutable bestPath = ""
        let mutable bestScore = 0

        for p in paths |> Seq.where (fun p -> p.Length = shortestPathFound.Value) do
            let mutable score = 0

            for i in 1 .. p.Length - 1 do
                if (p[i] = p[i - 1]) then
                    score <- score + 1

            if score > bestScore then
                bestPath <- p
                bestScore <- score

        bestPath
        *)

type pad =
    | NPAD
    | DPAD

let solve () =
    let io = aocIO
    let inp = io.getInput ()


    let npad =
        [| [| "7"; "8"; "9" |]
           [| "4"; "5"; "6" |]
           [| "1"; "2"; "3" |]
           [| "X"; "0"; "A" |] |]

    let npadGetV (p: Point2D) = (npad[p.y][p.x])

    let mutable npadButtonPositions = Dictionary<string, Point2D>()

    for y in 0 .. npad.Length - 1 do
        for x in 0 .. (npad[0].Length) - 1 do
            let p = Point2D(x, y)
            npadButtonPositions.Add(npadGetV p, p)

    let dpad = [| [| "X"; "^"; "A" |]; [| "<"; "v"; ">" |] |]
    let dpadGetV (p: Point2D) = (dpad[p.y][p.x])

    let mutable dpadButtonPositions = Dictionary<string, Point2D>()

    for y in 0 .. dpad.Length - 1 do
        for x in 0 .. dpad[0].Length - 1 do
            let p = Point2D(x, y)
            dpadButtonPositions.Add(dpadGetV p, p)

    let npadSp = Point2D(2, 3)
    let dpadSp = Point2D(2, 0)

    let offsets =
        [ ("<", (-1, 0)); (">", (1, 0)); ("^", (0, -1)); ("v", (0, 1)) ]
        |> List.map (fun x -> (fst x, snd x |> Point2D))

    let getPathScoreOriginal (p: string) =
        let mutable score = Int32.MaxValue

        for x in dpadButtonPositions.Keys do
            let mutable lp = None

            for i in 0 .. p.Length - 1 do
                if p[i].ToString() = x then
                    if lp.IsSome then
                        let diff = abs (i - lp.Value) - 1
                        score <- score - diff

                    lp <- Some(i)

        score

    let getBestScorePath (paths: string seq) =
        let mutable bestPath = ""
        let mutable bestScore = Int32.MinValue

        for p in paths do
            let score = getPathScoreOriginal p

            if score > bestScore then
                bestPath <- p
                bestScore <- score

        bestPath

    let c = Dictionary<string * Point2D, string list>()

    let shortestPathsToPressButton (button: string) startPos boundX boundY fn (pad: pad) =
        if c.ContainsKey((button, startPos)) then
            c[(button, startPos)]
        else
            let isPointWithinBoundaries (p: Point2D) =
                (p.y >= 0 && p.y < boundY && p.x >= 0 && p.x < boundX)

            let q = PriorityQueue()
            q.Enqueue((startPos, "", button), 0)

            let cons = HashSet()

            let mutable paths = []
            let mutable shortestPathFound = None

            while (q.Count > 0) do
                let cp, cpath, leftToProcess = q.Dequeue()

                let button = leftToProcess[0].ToString()

                if (leftToProcess.Length = 1 && (fn cp) = button) then
                    if (shortestPathFound.IsNone || shortestPathFound.Value > cpath.Length) then
                        shortestPathFound <- Some(cpath.Length)

                    paths <- paths @ [ cpath ]
                else if ((fn cp) = button) then
                    q.Enqueue((cp, cpath, leftToProcess[1..]), cpath.Length)
                else if (shortestPathFound.IsNone || cpath.Length < shortestPathFound.Value) then
                    offsets
                    |> Seq.where (fun (_, offs) -> isPointWithinBoundaries (offs + cp) && fn (offs + cp) <> "X")
                    |> Seq.iter (fun (ch, offs) ->
                        let nextPoint = offs + cp
                        let newPath = cpath + ch.ToString()

                        if
                            not (cons.Contains(newPath))
                            && (shortestPathFound.IsNone || cpath.Length <= shortestPathFound.Value)
                        then
                            cons.Add(newPath) |> ignore
                            let newPath = cpath + ch.ToString()
                            q.Enqueue((nextPoint, newPath, leftToProcess), cpath.Length))

            if (pad = DPAD) then
                c[(button, startPos)] <- paths

            paths

    let cached = Dictionary<(Point2D * string), (string * int) list>()

    let getPathsForSeq (dseq: string) (pad: pad) =
        let startPos =
            match pad with
            | NPAD -> npadSp
            | DPAD -> dpadSp

        let mutable sliceSize =
            if (dseq.Length % 2 = 0) then
                (dseq.Length / 3)
            else
                (dseq.Length / 2)

        if (pad = NPAD) then
            sliceSize <- 1

        sliceSize <- 1
        let q = PriorityQueue()
        q.Enqueue((startPos, "", dseq, sliceSize), 0)

        let mutable paths = List()
        let mutable shortestPath = None

        while (q.Count > 0) do
            let cp, cpath, dpadSeq, sliceSize = q.Dequeue()

            let mutable sliceSize = sliceSize

            if (dpadSeq.Length < sliceSize) then
                sliceSize <- dpadSeq.Length

            if (dpadSeq.Length = 0) then
                if (shortestPath.IsNone || shortestPath.Value > cpath.Length) then
                    shortestPath <- Some(cpath.Length)

                paths.Add(cpath)
            else if (shortestPath.IsNone || cpath.Length <= shortestPath.Value) then
                let target = dpadSeq[0 .. sliceSize - 1]

                let lastCh =
                    if (target.Length > 1) then
                        target.Substring(target.Length - 1, 1)
                    else
                        target[0] |> string

                let targetPos =
                    match pad with
                    | NPAD -> npadButtonPositions[lastCh]
                    | DPAD -> dpadButtonPositions[lastCh]

                let paths =
                    if cached.ContainsKey((cp, dpadSeq)) then
                        cached[(cp, target)]
                    else
                        let res =
                            match pad with
                            | DPAD -> shortestPathsToPressButton target cp (dpad[0].Length) dpad.Length dpadGetV pad
                            | NPAD -> shortestPathsToPressButton target cp (npad[0].Length) npad.Length npadGetV pad

                        let res = res |> List.map (fun x -> (x, getPathScoreOriginal x))
                        let maxScore = res |> List.maxBy snd |> snd
                        let paths = res |> List.where (fun x -> snd x = maxScore)
                        cached[(cp, target)] <- paths
                        paths

                paths
                |> Seq.iter (fun (p, score) ->
                    let newPath = cpath + p + "A"
                    q.Enqueue((targetPos, newPath, dpadSeq[sliceSize..], sliceSize), score * -1))

        paths

    let codes = inp

    let rec getAns1 () =
        let mutable a1 = bigint 0

        for code in codes do
            c.Clear()
            cached.Clear()

            let paths1 = getPathsForSeq code NPAD
            let paths2 = paths1 |> Seq.map (fun p -> getPathsForSeq p DPAD) |> Seq.collect id
            let paths3 = paths2 |> Seq.map (fun p -> getPathsForSeq p DPAD) |> Seq.collect id

            let bestPath = paths3 |> Seq.minBy _.Length

            let len = bestPath.Length

            let codeN =
                code
                |> String.extractAllNums
                |> Seq.map (_.ToString())
                |> String.concat ""
                |> Int32.Parse

            a1 <- a1 + bigint (len * codeN)

        a1

    for code in codes do
        let paths1 = getPathsForSeq code NPAD

        let paths2 = paths1 |> Seq.map (fun p -> getPathsForSeq p DPAD) |> Seq.collect id

        let mutable paths = paths2
        let bp = paths |> Seq.minBy (_.Length)
        printfn $"%A{bp.Length}"

        for i in 0..25 do
            printfn $"Starting %A{(i, paths |> Seq.length)}"

            paths <-
                paths
                |> Seq.map (fun p -> getPathsForSeq p DPAD)
                |> Seq.collect id
                |> Seq.toList

            let bp = paths |> Seq.minBy (_.Length)
            printfn $"%A{(bp.Length, paths |> Seq.length)}"
            ()

    printfn $"%A{(getAns1 ())}"

    0
