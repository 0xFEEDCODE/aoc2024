module aoc2024.day21XYASeAW

open System
open System.Collections.Generic
open Microsoft.FSharp.Core
open aoc2024.util

type pad =
    | NPAD
    | DPAD

type Entry =
    | Value of string
    | Indexes of (Point2D * string) list

type Movement = Point2D * Point2D * bigint

module M =
    let S m =
        let (s, e, l) = m
        s

    let E m =
        let (s, e, l) = m
        e

    let L m =
        let (s, e, l) = m
        l

let solve () =
    let io = aocIO
    let inp = io.getInput ()
    let codes = inp

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

    let om =
        Map.empty
            .Add(Point2D(-1, 0), "<")
            .Add(Point2D(1, 0), ">")
            .Add(Point2D(0, -1), "^")
            .Add(Point2D(0, 1), "v")


    let isPointWithinBoundaries (p: Point2D) (pad: pad) =
        let fn = if pad = DPAD then dpadGetV else npadGetV
        let boundX = if pad = DPAD then dpad[0].Length else npad[0].Length
        let boundY = if pad = DPAD then dpad.Length else npad.Length
        (p.y >= 0 && p.y < boundY && p.x >= 0 && p.x < boundX) && (fn p) <> "X"

    let cached = Dictionary<(string * Point2D), (Point2D Option * Point2D * string)>()

    let shortestMULTIPLEPaths (button: string) startPos boundX boundY fn =
        let isPointWithinBoundaries (p: Point2D) =
            (p.y >= 0 && p.y < boundY && p.x >= 0 && p.x < boundX)

        let q = Queue()
        q.Enqueue((startPos, ""))

        let cons = HashSet()

        let mutable paths = []
        let mutable shortestPathFound = None

        while (q.Count > 0) do
            let cp, cpath = q.Dequeue()

            if ((fn cp) = button) then
                if (shortestPathFound.IsNone || shortestPathFound.Value > cpath.Length) then
                    shortestPathFound <- Some(cpath.Length)

                paths <- paths @ [ cpath ]
            else
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
                        q.Enqueue((nextPoint, cpath + ch.ToString())))

        paths

    let shortestMultiNPAD (button: String) sp =
        shortestMULTIPLEPaths button sp (npad[0].Length) npad.Length npadGetV

    let shortestMultiDPAD (button: String) sp =
        shortestMULTIPLEPaths button sp (dpad[0].Length) dpad.Length dpadGetV

    let getPathsForSeq (seq: string) (pad: pad) =
        let startPos =
            match pad with
            | NPAD -> npadSp
            | DPAD -> dpadSp

        let q = Queue()
        q.Enqueue((startPos, "", [], seq))

        let mutable paths = List()
        let mutable shortestPathFound = None

        while (q.Count > 0) do
            let cp, cpath, fpath, seq = q.Dequeue()

            if (seq.Length = 0) then
                if (shortestPathFound.IsNone || cpath.Length < shortestPathFound.Value) then
                    shortestPathFound <- Some(cpath.Length)

                paths.Add(fpath)
            else if (shortestPathFound.IsNone || cpath.Length <= shortestPathFound.Value) then
                let target = seq[0] |> string

                let targetPos =
                    match pad with
                    | NPAD -> npadButtonPositions[target]
                    | DPAD -> dpadButtonPositions[target]

                let paths =
                    match pad with
                    | DPAD -> shortestMultiDPAD target cp
                    | NPAD -> shortestMultiNPAD target cp

                paths
                |> Seq.iter (fun p ->
                    let newPath = cpath + p + "A"
                    q.Enqueue((targetPos, newPath, fpath @ [ p + "A" ], seq[1..])))

        paths

    let shortestPathToPressButton (button: string) (startPos: Point2D) (dirOffs: Option<Point2D>) (padType: pad) =
        if cached.ContainsKey(button, startPos) then
            cached[button, startPos]
        else
            let q = PriorityQueue()
            q.Enqueue((startPos, "", dirOffs), 0)

            let mutable path = ""
            let mutable ldir = None
            let mutable lp = None

            let fn = if padType = DPAD then dpadGetV else npadGetV

            while (q.Count > 0) do
                let cp, cpath, dir = q.Dequeue()

                if ((fn cp) = button) then
                    lp <- Some(cp)
                    path <- cpath
                    ldir <- dir
                    q.Clear()
                else
                    if dir.IsSome && isPointWithinBoundaries (dir.Value + cp) padType then
                        let nextPoint = dir.Value + cp
                        let newPath = cpath + om[dir.Value].ToString()
                        q.Enqueue((nextPoint, newPath, dir), 0)

                    offsets
                    |> Seq.where (fun (_, offs) -> isPointWithinBoundaries (offs + cp) padType)
                    |> Seq.iter (fun (ch, offs) ->
                        let nextPoint = offs + cp
                        let newPath = cpath + ch.ToString()
                        q.Enqueue((nextPoint, newPath, Some(offs)), newPath.Length))

            cached[(button, startPos)] <- (ldir, (if lp.IsSome then lp.Value else startPos), path)
            (ldir, (if lp.IsSome then lp.Value else startPos), path)


    let mappedNpad = Dictionary<Point2D * string, Point2D option * Point2D * string>()

    let cachedMovements = HashSet<Movement>()

    let getAllAt p =
        cachedMovements |> Seq.where (fun (s, _, __) -> s = p)

    let getBestMatch s e =
        cachedMovements
        |> Seq.where (fun (s1, e2, __) -> s = s1 && e = e2)
        |> Seq.maxBy (fun (_, __, l) -> l)

    let mutable a1 = bigint 0

    for p in npadButtonPositions.Values do
        for ch in npadButtonPositions.Keys do
            if ch <> "X" then
                mappedNpad[(p, ch.ToString())] <- (shortestPathToPressButton ch p None NPAD)

    cached.Clear()

    let buttonAPos = dpadButtonPositions["A"]

    for p in dpadButtonPositions.Values do
        for ch in dpadButtonPositions.Keys do
            if ch <> "X" then
                let (ld, lp, path) = (shortestPathToPressButton ch p None DPAD)
                cachedMovements.Add(Movement(p, lp, path.Length)) |> ignore

    for code in codes do
        printfn $"%A{code}"

        let minLen =
            let mutable dpadSeq = getPathsForSeq code NPAD |> Seq.map id

            let mutable plane1Cp = buttonAPos

            let movements =
                dpadSeq
                |> Seq.map (fun l ->
                    l
                    |> List.map (fun path ->
                        let mutable movementLen = ZERO
                        let sp = buttonAPos

                        for move in path |> Seq.takeWhile (fun x -> x <> 'A') do
                            let ce = dpadButtonPositions[move |> string]
                            let movement = getBestMatch plane1Cp ce
                            movementLen <- movementLen + (movement |> M.L)
                            plane1Cp <- movement |> M.E

                        let movement = Movement(sp, plane1Cp, movementLen)
                        //cachedMovements.Add(movement)

                        movement)
                    |> List.take 1)


                |> Seq.toList

            let mutable shortestLenFound = None

            let n = 2

            for m in movements do
                printfn $"%A{m}"

                let mutable movementsToProcess = m
                let mutable len = ZERO

                for i in 0 .. n - 1 do
                    let mutable nextMovements = []
                    let mutable localLen = ZERO

                    for (s1, e1, l1) in movementsToProcess do
                        printfn $"%A{(dpadGetV s1, dpadGetV e1)}"
                        let movementToTarget = getBestMatch buttonAPos e1
                        let movementToA = getBestMatch (movementToTarget |> M.E) buttonAPos

                        localLen <- localLen + (movementToTarget |> M.L) + (movementToA |> M.L)
                        nextMovements <- nextMovements @ [ movementToTarget ]
                        nextMovements <- nextMovements @ [ movementToA ]

                    len <- len + localLen

                    movementsToProcess <- nextMovements

                if (shortestLenFound.IsNone || shortestLenFound.Value > len) then
                    shortestLenFound <- Some(len)

                printfn $"%A{len}"

            shortestLenFound

        let codeN =
            code
            |> String.extractAllNums
            |> Seq.map (_.ToString())
            |> String.concat ""
            |> Int32.Parse

        a1 <- a1 + (bigint codeN * minLen.Value)
        printfn $"Shortest found - %A{minLen}"

    printfn $"%A{a1}"
    ()
