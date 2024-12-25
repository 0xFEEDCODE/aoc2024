module aoc2024.day21F8

open System
open System.Collections.Generic
open System.Text
open Microsoft.FSharp.Core
open aoc2024.util

type pad =
    | NPAD
    | DPAD

type Entry =
    | Value of string
    | Indexes of (Point2D * string) list

type Movement = Point2D * Point2D * int

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


    let mappedDPAD = Dictionary<Point2D * string, Point2D * string>()

    let mutable a1 = bigint 0

    let mutable i = 0

    let patternMap = Dictionary<string, string>()
    patternMap.Add("<vA", "┌")
    patternMap.Add(">^A", "┘")
    patternMap.Add(">vA", "┐")
    patternMap.Add("^>A", "┌")
    patternMap.Add("<<A", "←")
    patternMap.Add(">>A", "→")
    patternMap.Add("^A", "^")
    patternMap.Add("<A", "<")
    patternMap.Add("vA", "v")
    patternMap.Add(">A", ">")
    patternMap.Add("A", "a")
    patternMap.Add("AA", "A")
    patternMap.Add("v>>A", "╔")
    patternMap.Add("v<A", "R")
    patternMap.Add("v<<A", "♦")
    patternMap.Add(">>^A", "♠")
    patternMap.Add(">^^A", "╝")
    patternMap.Add("<^A", "J")
    patternMap.Add("vvvA", "I")
    patternMap.Add("^>^A", "X")
    patternMap.Add("^^>A", "Y")
    patternMap.Add("^", "U")
    patternMap.Add("v", "D")
    patternMap.Add("<", "L")
    patternMap.Add(">", "R")

    //╔╗╝╚


    let getPattern x = patternMap[x]

    let decodePattern (x: string) =
        (patternMap |> Seq.find (fun kv -> kv.Value = x)).Key

    let patternToString (x: string) =
        let sb = StringBuilder()

        let matched = (patternMap |> Seq.find (fun kv -> kv.Value = x)).Key

        for ch in matched do
            sb.Append(ch) |> ignore

        if (sb.Length = 0) then
            ()

        sb.ToString()

    let decodeMultiplePatterns (x: string) =
        x
        |> Seq.map (fun y -> decodePattern (y.ToString()))
        |> Seq.fold (fun acc x -> acc + x) ""

    cached.Clear()

    for p in dpadButtonPositions.Values do
        for ch in dpadButtonPositions.Keys do
            if ch <> "X" then
                let (ld, lp, path) = (shortestPathToPressButton ch p None DPAD)

                mappedDPAD[(p, getPattern ch)] <-
                    i <- i + 1
                    (lp, getPattern (path + "A"))

    let processDs ds =
        ds
        |> List.fold
            (fun (plane1CP, pathAcc, pathLen) (pathToProcess: string) ->
                let mutable plane1CP = plane1CP
                let plane1CPSp = plane1CP

                let mutable plane2Path = ""
                let mutable processedPath = ""

                let mutable leftToProcess = pathToProcess
                let mutable len = pathLen

                while (leftToProcess.Length > 0) do
                    let bestMatch =
                        mappedDPAD
                        |> Seq.where (fun (x) -> fst x.Key = plane1CP && leftToProcess.StartsWith((snd x.Key)))
                        |> Seq.maxBy (fun (x) -> (snd x.Key) |> Seq.length)

                    let kv = bestMatch
                    let p1EntryCP, p1Path = kv.Key
                    let p2CP, p2Path = kv.Value

                    let decodedP2Path = decodeMultiplePatterns p2Path
                    len <- len + decodedP2Path.Length

                    processedPath <- processedPath + leftToProcess[.. (decodedP2Path.Length)]
                    plane2Path <- plane2Path + p2Path

                    mappedDPAD[(plane1CP, p1Path)] <- (p2CP, p2Path)
                    mappedDPAD[(plane1CPSp, processedPath)] <- (p2CP, plane2Path)

                    plane1CP <- dpadButtonPositions[(decodePattern p1Path) |> Seq.last |> string]

                    leftToProcess <- leftToProcess[(decodedP2Path.Length + 1) ..]

                (dpadButtonPositions[decodeMultiplePatterns (plane2Path) |> Seq.last |> string], pathAcc @ [ plane2Path ], len))
            (dpadSp, [], 0)

    for code in codes do
        printfn $"%A{code}"

        let mutable plane1CP = dpadButtonPositions["A"]

        let minLen =
            let mutable dpadSeq =
                (getPathsForSeq code NPAD)
                |> Seq.map (fun l ->
                    l
                    |> Seq.map (fun str ->
                        let sp = plane1CP

                        let p =
                            (str
                             |> Seq.map (fun ch ->
                                 let (_, lp, path) = (shortestPathToPressButton (ch.ToString()) plane1CP None DPAD)
                                 plane1CP <- lp
                                 //mappedDPAD[(plane1CP, getPattern (ch.ToString()))] <- (lp, path)
                                 path + "A")
                             |> Seq.fold (fun acc x -> acc + patternMap[x]) "")

                        //mappedDPAD[(sp, getPattern str)] <- (plane1CP, p)
                        p)
                    |> Seq.toList)
                |> Seq.toList

            plane1CP <- dpadButtonPositions["A"]

            dpadSeq <-
                (getPathsForSeq code NPAD)
                |> Seq.map (fun l ->
                    l
                    |> Seq.map (fun str ->
                        let sp = plane1CP

                        let p =
                            (str
                             |> Seq.map (fun ch ->
                                 let (_, lp, path) = (shortestPathToPressButton (ch.ToString()) plane1CP None DPAD)
                                 plane1CP <- lp
                                 mappedDPAD[(plane1CP, getPattern (ch.ToString()))] <- (lp, path)
                                 path + "A")
                             |> Seq.fold (fun acc x -> acc + patternMap[x]) "")

                        mappedDPAD[(sp, getPattern str)] <- (plane1CP, p)
                        p)
                    |> Seq.toList)
                |> Seq.toList

            let n = 4

            let mutable shortestLenFound = None

            for ds in dpadSeq do
                let mutable ds = ds

                let mutable len = 0

                for i in 0 .. n - 1 do
                    let (_, newDs, dsLen) = processDs ds
                    len <- dsLen
                    ds <- newDs

                    if (shortestLenFound.IsNone || shortestLenFound.Value > len) then
                        shortestLenFound <- Some(len)

            shortestLenFound

        let codeN =
            code
            |> String.extractAllNums
            |> Seq.map (_.ToString())
            |> String.concat ""
            |> Int32.Parse

        a1 <- a1 + bigint (codeN * minLen.Value)
        printfn $"Shortest found - %A{minLen}"

    printfn $"%A{a1}"
    ()
