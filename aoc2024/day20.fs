module aoc2024.day20

open System.Collections.Generic
open aoc2024.util

let solve () =
    let io = aocIO
    let inp = io.getInput ()

    let gr = inp |> Grid.initializeFromStringSeq
    let rows = gr.Length - 1
    let cols = gr[0].Length - 1

    let mutable distToEnd =
        gr
        |> Seq.map (fun x -> x |> Seq.map (fun _ -> -1) |> Seq.toArray)
        |> Seq.toArray

    let mutable cheats =
        gr
        |> Seq.map (fun x -> x |> Seq.map (fun _ -> List<int * Point2D>()) |> Seq.toArray)
        |> Seq.toArray

    let mutable start = Point2D()
    let mutable target = Point2D()

    for y in 0..rows do
        for x in 0..cols do
            if gr[y][x] = 'S' then
                start <- Point2D(x, y)

            if gr[y][x] = 'E' then
                target <- Point2D(x, y)

    let neighbourOffsets = [ (-1, 0); (1, 0); (0, -1); (0, 1) ] |> List.map Point2D

    let pr v =
        for y in 0..rows do
            for x in 0..cols do
                if (v |> List.contains (Point2D(x, y))) then
                    printf $"O"
                else
                    printf $"%c{(gr[y][x])}"

            printfn ""

        printfn ""

    let isPointWithinBoundaries (p: Point2D) =
        (p.y >= 0 && p.y <= rows && p.x >= 0 && p.x <= cols)

    let mapCheats start (cheatTime: int) (shortestAllowedTime: int) (initialDist: int) =
        let reached = HashSet()
        reached.Add((start, ZERO)) |> ignore

        //cp - Current Point
        //np - Next Point
        //nt - Next Time
        //ctl - Cheat time left
        
        [ (start, ZERO, cheatTime) ]
        |> List.unfold (fun items ->
            let newItems =
                items
                |> List.where (fun (_, time, ctl) -> (ctl > 0 && time < shortestAllowedTime))
                |> List.map (fun (cp, time, ctl) ->
                    let nt = time + ONE

                    neighbourOffsets
                    |> List.map ((+) cp)
                    |> List.where (fun np -> isPointWithinBoundaries np && not (reached.Contains((np, nt))))
                    |> List.map (fun np ->
                        if
                            gr[np.y][np.x] <> '#'
                            && (initialDist + cheatTime + distToEnd[np.y][np.x] <= shortestAllowedTime)
                        then
                            // figure out why - 1 !!!
                            (cheats[start.y][start.x]).Add((nt - 1, np))

                        reached.Add((np, nt)) |> ignore
                        Some(np, nt, ctl - ONE)))
                |> List.collect id
                |> List.map _.Value

            if newItems.IsEmpty then None else Some(None, newItems))
        |> List.iter (fun _ -> ())

    let findShortestPath (start: Point2D) (target: Point2D) =
        let mutable visited = Dictionary<Point2D, int>()

        let mutable q = PriorityQueue()

        q.Enqueue((start, ZERO), 0)

        while (q.Count > 0) do
            let mutable cp, time = q.Dequeue()

            if cp = target then
                if (distToEnd[start.y][start.x] < time) then
                    distToEnd[start.y][start.x] <- time

            else if (not (visited.ContainsKey(cp)) || (time < visited[cp])) then
                visited[cp] <- time

                let newTime = time + ONE

                neighbourOffsets
                |> List.iter (fun offs ->
                    let np = (offs + cp)

                    if (isPointWithinBoundaries np && gr[np.y][np.x] <> '#') then
                        q.Enqueue((np, newTime), newTime + np.GetManhattanDistance(target)))

        distToEnd[start.y][start.x]

    let findPaths (start: Point2D) (target: Point2D) (shortestAllowedTime: int) =
        let mutable lastId = ZERO
        let mutable results = List()
        let mutable reached = Dictionary<Point2D, int>()

        let mutable st = Stack()

        st.Push((start, ONE, 0UL))

        while (st.Count > 0) do
            let cp, time, id = st.Pop()

            if cp = target then
                results.Add(time)
            else if (time < shortestAllowedTime && (not (reached.ContainsKey(cp)))) then
                reached[cp] <- time

                let mutable cons = Dictionary<Point2D * Point2D, int>()

                cheats[cp.y][cp.x]
                |> Seq.iter (fun (cheatTime, cheatEnd) ->
                    let timeToReach = time + cheatTime + distToEnd[cheatEnd.y][cheatEnd.x]

                    if (timeToReach <= shortestAllowedTime && not (cons.ContainsKey(cp, cheatEnd))) then
                        cons[(cp, cheatEnd)] <- ZERO

                        let id =
                            lastId <- lastId + ONE
                            lastId

                        st.Push(target, timeToReach, id))

                neighbourOffsets
                |> List.iter (fun offs ->
                    let np = (offs + cp)

                    if (np.y >= 0 && np.y <= rows && np.x >= 0 && np.x <= cols) then
                        if gr[np.y][np.x] <> '#' then
                            st.Push((np, time + ONE, id)))

        results

    let ctime = 20

    let shortestPathWithoutCheats = findShortestPath start target

    for y in 1 .. rows - 1 do
        for x in 1 .. cols - 1 do
            let p = Point2D(x, y)

            if (gr[y][x] <> '#') then
                findShortestPath p target |> ignore

    for y in 0..rows do
        for x in 0..cols do
            let p = Point2D(x, y)

            if gr[y][x] <> '#' then
                let initDist = distToEnd[start.y][start.x] - distToEnd[y][x]
                mapCheats p ctime shortestPathWithoutCheats initDist

    printfn $"%A{shortestPathWithoutCheats}"

    let nx = 100

    let allPathsWithCheats =
        findPaths start target (shortestPathWithoutCheats - nx)
        |> Seq.toList
        |> List.map ((-) shortestPathWithoutCheats)
        |> List.groupBy id

    let mutable a = ZERO
    let mutable a2 = ZERO

    for (savesSeconds, cheatsN) in allPathsWithCheats do

        let cheatsN = cheatsN |> Seq.length

        if (savesSeconds >= nx) then
            printfn $"%A{(savesSeconds, cheatsN)}"
            a <- a + ONE
            a2 <- a2 + cheatsN

    printfn $"%A{a}"
    printfn $"%A{a2}"

    (*
    printfn $"%A{(start, target)}"
    printfn $"%A{r}"
    *)



    0
