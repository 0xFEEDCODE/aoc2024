﻿module aoc2024.dayxy

open System
open System.Collections.Generic
open System.Text
open Microsoft.FSharp.Core
open aoc2024.util

type pad =
    | NPAD
    | DPAD

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

                paths.Add(cpath)
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
                    q.Enqueue((targetPos, newPath, fpath @ [ newPath ], seq[1..])))

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

            //cached[(button, startPos)] <- (ldir, (if lp.IsSome then lp.Value else startPos), path)
            (ldir, (if lp.IsSome then lp.Value else startPos), path)


    let mappedNpad = Dictionary<(Point2D * string), Point2D option * Point2D * string>()

    let mappedDPAD = Dictionary<(Point2D * string), Point2D option * Point2D * string>()

    let mutable a1 = bigint 0

    for p in npadButtonPositions.Values do
        for ch in npadButtonPositions.Keys do
            if ch <> "X" then
                mappedNpad[(p, ch.ToString())] <- (shortestPathToPressButton ch p None NPAD)

    cached.Clear()

    for p in dpadButtonPositions.Values do
        for ch in dpadButtonPositions.Keys do
            if ch <> "X" then
                mappedDPAD[(p, ch.ToString())] <-
                    let (ld, lp, path) = (shortestPathToPressButton ch p None DPAD)
                    (ld, lp, path + "A")

    for code in codes do
        let spaths = getPathsForSeq code NPAD

        printfn $"%A{spaths}"
        exit (0)

        let mutable _, dpadSeq =
            code
            |> Seq.fold
                (fun (cp, pathAcc) button ->
                    let button = button.ToString()
                    let (_, __, path) = mappedNpad[cp, button.ToString()]
                    (npadButtonPositions[button], pathAcc @ [ [ path + "A" ] ]))
                (npadSp, [])

        printfn $"%A{dpadSeq}"
        let n = 2

        for i in 0 .. n - 1 do
            printfn $"%A{(i, mappedDPAD.Count)}"

            dpadSeq <-
                dpadSeq
                |> Seq.fold
                    (fun (plane1CP, pathAcc) paths ->
                        let lastPath = paths |> Seq.last
                        let mutable i = 0
                        let mutable stop = false
                        let mutable plane1CP = plane1CP

                        let plane1CPSp = plane1CP

                        let mutable processedTotalLen = 0
                        let mutable translations = []
                        let mutable translatedPath = StringBuilder()
                        let mutable processedPath = StringBuilder()

                        let mutable toTranslate = lastPath

                        while not stop do
                            i <- 0

                            while not stop && not (mappedDPAD.ContainsKey(plane1CP, toTranslate)) do
                                toTranslate <- toTranslate.Substring(0, lastPath.Length - i - processedTotalLen)
                                i <- i + 1

                                if ((lastPath.Length - i - processedTotalLen) < 0) then
                                    stop <- true

                            if not stop then
                                let plane2LastDir, plane2CP, translated = mappedDPAD[plane1CP, toTranslate]
                                translatedPath.Append(translated) |> ignore
                                processedPath.Append(toTranslate) |> ignore

                                translations <- translations @ [ (plane2LastDir, plane2CP, translated) ]
                                processedTotalLen <- processedTotalLen + toTranslate.Length

                                mappedDPAD[(plane1CP, toTranslate)] <- (plane2LastDir, plane2CP, translated)
                                mappedDPAD[(plane1CPSp, processedPath.ToString())] <- (plane2LastDir, plane2CP, translatedPath.ToString())

                                plane1CP <- dpadButtonPositions[toTranslate |> Seq.last |> string]
                                toTranslate <- lastPath[processedTotalLen..]

                            if toTranslate.Length = 0 then
                                stop <- true

                        if (processedTotalLen <> lastPath.Length) then
                            failwith "f"

                        let mutable newPath = translatedPath.ToString()

                        (dpadButtonPositions[newPath |> Seq.last |> string], pathAcc @ [ [ newPath ] ]))
                    (dpadSp, [])
                |> snd

        //v<<A>>^A<A>AvA<^AA>A<vAAA>^A
        //v<<A>^>A<A>AvA^A<vA^>A

        //printfn $"%A{dpadSeq}"

        let r =
            dpadSeq
            |> Seq.map (fun x -> x |> Seq.last)
            |> Seq.fold (fun acc x -> acc + x) ""

        printfn $"%A{(r.Length)}"

        let codeN =
            code
            |> String.extractAllNums
            |> Seq.map (_.ToString())
            |> String.concat ""
            |> Int32.Parse

        ()
        a1 <- a1 + bigint (codeN * r.Length)

    printfn $"%A{a1}"
    ()
