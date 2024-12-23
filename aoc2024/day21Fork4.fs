module aoc2024.dayeqw

open System
open System.Collections.Generic
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

    let om =
        Map.empty
            .Add(Point2D(-1, 0), '<')
            .Add(Point2D(1, 0), '>')
            .Add(Point2D(0, -1), '^')
            .Add(Point2D(0, 1), 'v')

    let offsetMap =
        Map.empty
            .Add('<', Point2D(-1, 0))
            .Add('>', Point2D(1, 0))
            .Add('^', Point2D(0, -1))
            .Add('v', Point2D(0, 1))

    let counterDir =
        Map.empty
            .Add(Point2D(1, 0), Point2D(-1, 0))
            .Add(Point2D(-1, 0), Point2D(1, 0))
            .Add(Point2D(0, 1), Point2D(0, -1))
            .Add(Point2D(0, -1), Point2D(0, 1))

    let isPointWithinBoundaries (p: Point2D) boundX boundY fn =
        (p.y >= 0 && p.y < boundY && p.x >= 0 && p.x < boundX) && (fn p) <> 'X'

    let cached = Dictionary<Point2D * char, string>()

    let getPathScoreOriginal (p: string) =
        let mutable score = Int32.MaxValue

        for x in dpadButtonPositions.Keys do
            let mutable lp = None

            for i in 0 .. p.Length - 1 do
                if p[i] = x then
                    if lp.IsSome then
                        let diff = abs (i - lp.Value)
                        score <- score - diff

                    lp <- Some(i)

        score



    let shortestPathToPressButton (button: char) startPos boundX boundY fn =
        if (fn startPos) = button then
            ""
        else if not (cached.ContainsKey((startPos, button))) then
            let q = PriorityQueue()
            q.Enqueue((startPos, "", None), 0)

            let mutable path = ""
            let mutable paths = []
            let mutable shortestPath = None

            while (q.Count > 0) do
                let cp, cpath, dir = q.Dequeue()

                if ((fn cp) = button) then
                    path <- cpath

                    if (shortestPath.IsNone || shortestPath.Value > cpath.Length) then
                        shortestPath <- Some(cpath.Length)

                    paths <- paths @ [ path ]

                else if (shortestPath.IsNone || shortestPath.Value > cpath.Length) then
                    if (dir.IsSome && isPointWithinBoundaries (dir.Value + cp) boundX boundY fn) then
                        let nextPoint = dir.Value + cp
                        let newPath = cpath + om[dir.Value].ToString()
                        q.Enqueue((nextPoint, newPath, dir), newPath.Length)

                    offsets
                    |> Seq.where (fun (_, offs) -> isPointWithinBoundaries (offs + cp) boundX boundY fn)
                    |> Seq.iter (fun (ch, offs) ->
                        let nextPoint = offs + cp
                        let newPath = cpath + ch.ToString()
                        q.Enqueue((nextPoint, newPath, Some(offs)), newPath.Length))

            let bestMatch = paths |> Seq.maxBy getPathScoreOriginal
            cached[(startPos, button)] <- bestMatch
            bestMatch
        else
            cached[(startPos, button)]

    let limit = 10

    let spb (button: char) startPos boundX boundY fn =
        let rec loop (button: char) startPos boundX boundY fn level (acc: string) =
            if (fn startPos) = button then
                ""
            else if not (cached.ContainsKey((startPos, button))) then
                let q = PriorityQueue()
                q.Enqueue((startPos, "", None), 0)

                let mutable path = ""

                while (q.Count > 0) do
                    let cp, cpath, dir = q.Dequeue()

                    if ((fn cp) = button) || cached.ContainsKey(startPos, button) then
                        if (acc.Length > 0) then
                            path <- acc + "A" + cpath
                        else
                            path <- cpath

                        q.Clear()

                    else
                        offsets
                        |> Seq.where (fun (_, offs) ->
                            isPointWithinBoundaries (offs + cp) boundX boundY fn
                            && (dir.IsNone || counterDir[dir.Value] <> offs))
                        |> Seq.iter (fun (ch, offs) ->
                            let nextPoint = offs + cp
                            let newPath = cpath + ch.ToString()

                            let prio =
                                if level = limit then
                                    newPath.Length
                                else
                                    let p = (loop (newPath |> Seq.last) dpadSp boundX boundY fn (level + 1) "")
                                    ()
                                    p |> Seq.length

                            q.Enqueue((nextPoint, newPath, Some(offs)), prio))

                //cached[(startPos, button)] <- path
                path
            else
                cached[(startPos, button)]

        loop button startPos boundX boundY fn 0 ""

    let translate dseq sp =
        let mutable cp = sp
        let mutable path = ""

        for ch in dseq do
            let newPath = (shortestPathToPressButton ch cp dpad[0].Length dpad.Length dpadGetV)
            path <- path + newPath + "A"
            cp <- dpadButtonPositions[ch]

        (path, cp)

    let mutable cp = npadSp

    let mutable a1 = 0

    for code in codes do

        let mutable currentPath =
            code
            |> Seq.map (fun ch ->
                let path = (shortestPathToPressButton ch cp npad[0].Length npad.Length npadGetV)
                cp <- npadButtonPositions[ch]
                path)
            |> Seq.fold (fun acc x -> acc + x + "A") ""

        let n = 10

        for i in 0..n do
            cached.Clear()
            let mutable cp = dpadSp
            let mutable newPath = ""

            for ch in currentPath do
                let transl, np = translate (ch.ToString()) cp
                newPath <- newPath + transl
                cp <- np

            currentPath <- newPath

        let codeN =
            code
            |> String.extractAllNums
            |> Seq.map (_.ToString())
            |> String.concat ""
            |> Int32.Parse

        //printfn $"%A{currentPath}"
        a1 <- a1 + (currentPath.Length * codeN)
        printfn $"%A{(currentPath.Length, codeN)}"

    printfn $"%A{a1}"
