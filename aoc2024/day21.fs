module aoc2024.day21

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

    let isPointWithinBoundaries (p: Point2D) (pad: pad) =
        let fn = if pad = DPAD then dpadGetV else npadGetV
        let boundX = if pad = DPAD then dpad[0].Length else npad[0].Length
        let boundY = if pad = DPAD then dpad.Length else npad.Length
        (p.y >= 0 && p.y < boundY && p.x >= 0 && p.x < boundX) && (fn p) <> "X"

    let shortestMULTIPLEPaths (button: string) startPos boundX boundY fn =
        let isPointWithinBoundaries (p: Point2D) =
            (p.y >= 0 && p.y < boundY && p.x >= 0 && p.x < boundX)

        let q = Queue()
        q.Enqueue((startPos, "", [ startPos ]))

        let mutable paths = []
        let mutable shortestPathFound = None

        while (q.Count > 0) do
            let cp, cpath, cons = q.Dequeue()

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
                        not (cons |> List.contains nextPoint)
                        && (shortestPathFound.IsNone || newPath.Length < shortestPathFound.Value + 2)
                    then
                        q.Enqueue((nextPoint, newPath, cons @ [ nextPoint ])))

        paths

    let shortestMultiNPAD (button: String) sp =
        shortestMULTIPLEPaths button sp (npad[0].Length) npad.Length npadGetV

    let shortestMultiDPAD (button: String) sp =
        shortestMULTIPLEPaths button sp (dpad[0].Length) dpad.Length dpadGetV

    let getPathsForSeqSp (seq: string) (pad: pad) startPos =

        let q = Queue()
        q.Enqueue((startPos, "", [], seq))

        let mutable paths = List()

        while (q.Count > 0) do
            let cp, cpath, fpath, seq = q.Dequeue()

            if (seq.Length = 0) then
                paths.Add(fpath)
            else
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

    let getPathsForSeq (seq: string) (pad: pad) =
        let startPos =
            match pad with
            | NPAD -> npadSp
            | DPAD -> dpadSp

        let q = Queue()
        q.Enqueue((startPos, "", [], seq))

        let mutable paths = List()

        while (q.Count > 0) do
            let cp, cpath, fpath, seq = q.Dequeue()

            if (seq.Length = 0) then
                paths.Add(fpath)
            else
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

    let patternTransformationMap = Dictionary<Point2D * string, string>()

    let rec proc (path: string) =

        let splitPath (path: string) =
            let sb = StringBuilder()
            let mutable paths = []

            for i in 0 .. path.Length - 1 do
                sb.Append(path[i]) |> ignore

                if path[i] = 'A' then
                    paths <- paths @ [ sb.ToString() ]
                    sb.Clear() |> ignore

            paths

        let incrPatternBy (d: Dictionary<string, bigint>) pattern x =
            if not (d.ContainsKey(pattern)) then
                d[pattern] <- x
            else
                d[pattern] <- d[pattern] + x

        let decrPatternBy (d: Dictionary<string, bigint>) pattern x =
            if not (d.ContainsKey(pattern)) then
                d[pattern] <- x
            else
                d[pattern] <- d[pattern] - x

        let ds = Dictionary<string, bigint>()

        let rec patternTransformation (p: Point2D) (x: string) =
            let key = (p, x)

            if patternTransformationMap.ContainsKey(key) then
                patternTransformationMap[key]
            else
                let spl = splitPath x
                let mutable path = ""

                for part in spl do
                    if not (patternTransformationMap.ContainsKey(dpadSp, part)) then
                        let mutable acc = ""
                        let mutable cp = dpadSp

                        for ch in part do
                            acc <- acc + patternTransformation cp (ch.ToString())
                            cp <- dpadButtonPositions[ch |> string]

                        patternTransformationMap[(dpadSp, part)] <- acc

                    path <- path + (patternTransformation dpadSp x)

                patternTransformationMap[key] <- path
                path

        for kv in dpadButtonPositions do
            for kv2 in dpadButtonPositions do
                let ch = kv2.Key

                if (ch <> "X") then
                    let pos = kv.Value

                    let score (x: string) =
                        let mutable p = x

                        for i in 0..2 do
                            p <- getPathsForSeqSp p DPAD pos |> Seq.head |> Seq.fold (fun a x -> a + x) ""

                        p.Length * -1





                    (*
                        for i in 1 .. x.Length - 1 do
                            if (x[i] = x[i - 1]) then prev <- prev + 1 else prev <- 0
                            score <- score + prev * 2

                        score
                        *)

                    let path =
                        getPathsForSeqSp ch DPAD pos
                        |> Seq.map (fun y -> y |> Seq.fold (fun a x -> a + x) "")
                        |> Seq.maxBy (score)

                    patternTransformationMap[(pos, ch)] <- path


        let incrBy x = incrPatternBy ds x
        let decrBy x = decrPatternBy ds x

        for p in splitPath path do
            incrBy p ONE

        let n = 25

        for i in 0 .. n - 2 do
            let dsValues = ds |> Seq.map (fun x -> (x.Key, x.Value)) |> Seq.toArray

            for k, v in dsValues do
                let split = splitPath (patternTransformation dpadSp k)

                for part in split do
                    incrBy part v

                decrBy k v

        let mutable len = ZERO

        for kv in ds do
            len <- len + (bigint kv.Key.Length * kv.Value)

        len

    let shortestPathToPressButton (button: string) (startPos: Point2D) (padType: pad) =
        let getPathScore (p: string) = (proc (p + "A"))

        let q = Queue()
        q.Enqueue((startPos, "", []))

        let mutable path = ""
        let mutable ldir = None
        let mutable lp = None

        let fn = if padType = DPAD then dpadGetV else npadGetV
        let mutable paths = []

        while (q.Count > 0) do
            let cp, cpath, visited = q.Dequeue()

            if ((fn cp) = button) then
                lp <- Some(cp)
                path <- cpath
                paths <- paths @ [ path ]
            else
                offsets
                |> Seq.where (fun (_, offs) ->
                    isPointWithinBoundaries (offs + cp) padType
                    && not (visited |> List.contains (offs + cp)))
                |> Seq.iter (fun (ch, offs) ->
                    let nextPoint = offs + cp
                    let newPath = cpath + ch.ToString()
                    q.Enqueue(nextPoint, newPath, visited @ [ cp ]))

        (ldir, (if lp.IsSome then lp.Value else startPos), paths |> Seq.minBy getPathScore)

    let mutable a1 = bigint 0

    for code in codes do
        let shortestLen =
            let mutable dpadSeq =
                (getPathsForSeq code NPAD)
                |> Seq.map (fun npadSeq ->
                    let mutable plane1CP = dpadButtonPositions["A"]

                    npadSeq
                    |> Seq.map (fun str ->
                        let p =
                            (str
                             |> Seq.map (fun ch ->
                                 let _, lp, path = (shortestPathToPressButton (ch.ToString()) plane1CP DPAD)
                                 plane1CP <- lp
                                 path + "A")
                             |> Seq.fold (fun acc x -> acc + x) "")

                        p)
                    |> Seq.toList)
                |> Seq.toList

            dpadSeq
            |> Seq.map (fun s -> proc (s |> Seq.fold (fun a x -> a + x) ""))
            |> Seq.min

        let codeN =
            code
            |> String.extractAllNums
            |> Seq.map (_.ToString())
            |> String.concat ""
            |> Int32.Parse

        a1 <- a1 + ((bigint codeN) * shortestLen)

    printfn $"%A{a1}"

    ()
