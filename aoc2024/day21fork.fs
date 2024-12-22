module aoc2024.dayX

open System
open System.Collections.Generic
open aoc2024.util


let tryGetValue k (d: Dictionary<_, _>) =
    match d.TryGetValue k with
    | true, v -> Some v
    | _ -> None

type Dir =
    | L
    | U
    | R
    | D

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

    let mutable dpadButtonIndexes = List()
    let mutable dpadButtonPositions = Dictionary<string, Point2D>()

    for y in 0 .. dpad.Length - 1 do
        for x in 0 .. dpad[0].Length - 1 do
            let p = Point2D(x, y)

            if (dpadGetV p) <> "X" then
                dpadButtonIndexes.Add(p)
                dpadButtonPositions.Add(dpadGetV p, p)

    let npadSp = Point2D(2, 3)
    let dpadSp = Point2D(2, 0)

    let offsetsMap =
        Map.empty
            .Add("<", Point2D(-1, 0))
            .Add(">", Point2D(1, 0))
            .Add("^", Point2D(0, -1))
            .Add("v", Point2D(0, 1))

    let offsets =
        [ ("<", (-1, 0)); (">", (1, 0)); ("^", (0, -1)); ("v", (0, 1)) ]
        |> List.map (fun x -> (fst x, snd x |> Point2D))

    let getBestScorePath (paths: string seq) =
        let mutable bestPath = ""
        let mutable bestScore = Int32.MinValue

        for p in paths do
            let mutable score = p.Length * -1

            let mutable samePrev = 0

            for i in 1 .. p.Length - 1 do
                if (p[i] = p[i - 1]) then
                    samePrev <- samePrev + 1
                    score <- score + (samePrev * 1)
                else
                    samePrev <- 0

            if score > bestScore then
                bestPath <- p
                bestScore <- score

        bestPath

    let shortestPathsToPressButton (button: string) startPos boundX boundY fn =
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

    let shortestPathToPressButtonNpad (button: String) sp =
        shortestPathsToPressButton button sp (npad[0].Length) npad.Length npadGetV

    let shortestPathToPressButtonDpad (button: String) sp =
        shortestPathsToPressButton button sp (dpad[0].Length) dpad.Length dpadGetV

    let getPathsForSeq (seq: string) (pad: pad) =
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
                let target = seq[0] |> string

                let targetPos =
                    match pad with
                    | NPAD -> npadButtonPositions[target]
                    | DPAD -> dpadButtonPositions[target]

                let paths =
                    match pad with
                    | DPAD -> shortestPathToPressButtonDpad target cp
                    | NPAD -> shortestPathToPressButtonNpad target cp

                paths
                |> Seq.iter (fun p ->
                    let newPath = cpath + p + "A"
                    q.Enqueue((targetPos, newPath, seq[1..])))

        paths


    let memo = Dictionary<(Point2D * string), string>()

    let getPathsForSeqPt2 (dpadSeq: string) (pad: pad) =
        let startPos =
            match pad with
            | NPAD -> npadSp
            | DPAD -> dpadSp

        let q = Queue()
        q.Enqueue((startPos, "", [], dpadSeq))

        let mutable paths = List()
        let mutable shortestPathFound = None

        while (q.Count > 0) do
            let cp, cpath, parts, dpadSeq = q.Dequeue()

            if (dpadSeq.Length = 0) then
                if (shortestPathFound.IsNone || cpath.Length < shortestPathFound.Value) then
                    shortestPathFound <- Some(cpath.Length)

                paths.Add((cpath, parts))
            else if (shortestPathFound.IsNone || cpath.Length <= shortestPathFound.Value) then
                let target = dpadSeq[0] |> string

                let targetPos =
                    match pad with
                    | NPAD -> npadButtonPositions[target]
                    | DPAD -> dpadButtonPositions[target]

                let n = 10000

                let memoized =
                    if memo.Count > 0 then
                        Seq.init n (fun x -> x + 1)
                        |> Seq.where (fun len -> len <= dpadSeq.Length)
                        |> Seq.map (fun len ->
                            let target = dpadSeq[0 .. len - 1]
                            (len, target, (tryGetValue (cp, target) memo)))
                        |> Seq.takeWhile (fun (_, __, result) -> result.IsSome)
                        |> Seq.tryLast
                    else
                        None

                if (memoized.IsSome) then
                    let len, target, result = memoized.Value
                    let newPath = cpath + result.Value + "A"

                    let targetPos =
                        match pad with
                        | NPAD -> npadButtonPositions[target[target.Length - 1].ToString()]
                        | DPAD -> dpadButtonPositions[target[target.Length - 1].ToString()]

                    q.Clear()
                    q.Enqueue((targetPos, newPath, parts @ [ (cp, result.Value, target) ], dpadSeq[len..]))
                else
                    let paths =
                        match pad with
                        | DPAD -> shortestPathsToPressButton target cp (dpad[0].Length) dpad.Length dpadGetV
                        | NPAD -> shortestPathsToPressButton target cp (npad[0].Length) npad.Length npadGetV

                    paths
                    |> Seq.iter (fun p ->
                        let newPath = cpath + p + "A"
                        q.Enqueue((targetPos, newPath, parts @ [ (cp, p, target) ], dpadSeq[1..])))

        paths

    let codes = inp


    let translate (sp: Point2D) (dir: string) =
        shortestPathToPressButtonDpad dir sp |> Seq.head

    let translateSeq (sp: Point2D) (dirs: string) =
        dirs
        |> Seq.fold
            (fun (sp, path) dir ->
                let dir = dir.ToString()
                let res = (translate sp dir) + "A"

                let np =
                    if res.Length > 0 then
                        dpadButtonPositions[(res[res.Length - 1].ToString())]
                    else
                        sp

                (np, path + res))
            (sp, "")
        |> snd

    let mutable lv0 =
        dpadButtonPositions.Values
        |> Seq.map (fun p -> dpadButtonPositions.Keys |> Seq.map (fun dir -> (p, dir, translate p dir)))
        |> Seq.skip 4
        |> Seq.collect id

    let res =
        (0, lv0)
        |> Seq.unfold (fun (idx, x) ->
            let n =
                x
                |> Seq.map (fun (pos, dir, path) ->
                    dpadButtonPositions.Values
                    |> Seq.map (fun sp -> (sp, path, path + translate sp path)))
                |> Seq.collect id

            if idx < 25 then Some(n, (idx + 1, n)) else None)
        |> Seq.toArray

    printfn $"%A{res}"

    (*
    for i in 0..25 do
        lv0 <- lv0 |> Seq.map (translateSeq dpadSp)
        *)


    exit (0)

    let mutable a1 = ZERO
    a1 <- ZERO

    for code in codes do
        let paths1 = getPathsForSeq code NPAD
        let p1 = getBestScorePath paths1
        let paths2 = getPathsForSeq p1 DPAD
        let p2 = getBestScorePath paths2
        let paths3 = getPathsForSeq p2 DPAD
        let bestPath = paths3 |> Seq.minBy _.Length

        let len = bestPath.Length

        let codeN =
            code
            |> String.extractAllNums
            |> Seq.map (_.ToString())
            |> String.concat ""
            |> Int32.Parse

        a1 <- a1 + bigint (len * codeN)
    (*
        printfn $"%A{(p1, p2, bestPath)}"
        printfn $"%A{(len * codeN)}"
        printfn $"%A{(len, codeN)}"
        *)

    printfn $"R1 - %A{a1}"

    a1 <- ZERO

    for code in codes do
        let paths1 = getPathsForSeq code NPAD
        let paths2 = paths1 |> Seq.map (fun x -> getPathsForSeq x DPAD) |> Seq.collect id

        let mutable paths = paths2

        for i in 0..25 do
            printfn $"Start %A{i}"
            let res = paths |> Seq.map (fun x -> getPathsForSeqPt2 x DPAD) |> Seq.collect id
            let bp = res |> Seq.minBy (fun x -> (fst x) |> Seq.length)
            paths <- [ fst bp ]
            printfn $"%A{(i, (fst bp) |> Seq.length)}"

            let mutable fp = ""
            let mutable fc = ""
            let mutable i = 0

            for pos, p, ch in (snd bp) do
                let ch = ch.ToString()

                fc <- fc + ch
                if i = 0 then fp <- p else fp <- fp + "A" + p
                memo[(pos, ch)] <- p

                if i > 0 then
                    memo[(pos, fc)] <- fp

                i <- i + 1

            printfn $"%A{memo.Count}"



        let paths2 = paths1 |> Seq.map (fun x -> (x, getPathsForSeq x DPAD))

        let paths3 =
            paths2
            |> Seq.map (fun (x, items) ->
                let newPaths = items |> Seq.map (fun y -> (x, y, getPathsForSeq y DPAD))
                newPaths)
            |> Seq.collect id

        let (p1, p2, bp) =
            paths3
            |> Seq.map (fun (x, y, z) -> (x, y, z |> Seq.minBy _.Length))
            |> Seq.minBy (fun (x, _, z) -> z.Length)

        printfn $"%A{(p1, p2, bp)}"
        let len = bp |> Seq.length

        let codeN =
            code
            |> String.extractAllNums
            |> Seq.map (_.ToString())
            |> String.concat ""
            |> Int32.Parse

        a1 <- a1 + bigint (len * codeN)
        printfn $"%A{(len * codeN)}"
        printfn $"%A{(len, codeN)}"

    printfn $"R2- %A{a1}"






    0
