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
        |> Seq.map (fun x -> x |> Seq.map (fun _ -> List()) |> Seq.toArray)
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

    let sp = start

    let findCheats (start: Point2D) (cheatTime: int) (shortestAllowedTime: int) (initialDist: int) =
        let q = Queue()
        q.Enqueue((start, 0, cheatTime))

        let mutable reached = Dictionary<Point2D * int, bool>()
        reached[(start, 0)] <- true

        while (q.Count > 0) do
            let cp, time, cheatTimeLeft = q.Dequeue()

            if (cheatTimeLeft > 0 && time < shortestAllowedTime) then
                let newTime = time + 1

                neighbourOffsets
                |> List.iter (fun offs ->
                    let np = (offs + cp)

                    if (np.y >= 0 && np.y <= rows && np.x >= 0 && np.x <= cols) then
                        if
                            gr[np.y][np.x] <> '#'
                            && (initialDist + cheatTime + distToEnd[np.y][np.x] <= shortestAllowedTime)
                        then
                            (cheats[start.y][start.x]).Add((newTime, np))

                            if (np = start) then
                                ()

                        if not (reached.ContainsKey((np, newTime))) then
                            q.Enqueue((np, newTime, cheatTimeLeft - 1))
                            reached[(np, newTime)] <- true)

        cheats[start.y][start.x]


    let findShortestPath (start: Point2D) (target: Point2D) =
        let mutable visited = Dictionary<Point2D, int>()

        let mutable q = PriorityQueue()

        q.Enqueue((start, 0), 0)

        while (q.Count > 0) do
            let mutable cp, time = q.Dequeue()

            if cp = target then
                if (distToEnd[start.y][start.x] < time) then
                    distToEnd[start.y][start.x] <- time

            else if (not (visited.ContainsKey(cp)) || (time < visited[cp])) then
                visited[cp] <- time

                let newTime = time + 1

                neighbourOffsets
                |> List.iter (fun offs ->
                    let np = (offs + cp)

                    if (np.y >= 0 && np.y <= rows && np.x >= 0 && np.x <= cols && gr[np.y][np.x] <> '#') then
                        q.Enqueue((np, newTime), newTime + np.GetManhattanDistance(target)))

        distToEnd[start.y][start.x]

    let fp (start: Point2D) (target: Point2D) (shortestAllowedTime: int) =
        let mutable lastId = 0UL
        let mutable results = List()
        let mutable reached = Dictionary<Point2D, int>()

        let mutable st = Stack()

        st.Push((start, 0, 0UL))

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
                        cons[(cp, cheatEnd)] <- 0

                        let id =
                            lastId <- lastId + ONE
                            lastId

                        st.Push(target, timeToReach, id))

                neighbourOffsets
                |> List.iter (fun offs ->
                    let np = (offs + cp)

                    if (np.y >= 0 && np.y <= rows && np.x >= 0 && np.x <= cols) then
                        if gr[np.y][np.x] <> '#' then
                            st.Push((np, time + 1, id)))

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
                let initdist = distToEnd[start.y][start.x] - distToEnd[y][x]
                findCheats p ctime shortestPathWithoutCheats initdist |> ignore

    printfn $"%A{shortestPathWithoutCheats}"

    let nx = 100

    let allPathsWithCheats =
        fp start target (shortestPathWithoutCheats - nx)
        |> Seq.toList
        |> List.map ((-) shortestPathWithoutCheats)
        |> List.groupBy id

    let mutable a = ZERO
    let mutable a2 = ZERO

    for (savesSeconds, cheatsN) in allPathsWithCheats do

        let cheatsN = cheatsN |> Seq.length

        if (savesSeconds >= nx) then
            printfn $"%A{(savesSeconds, cheatsN)}"
            a <- a + 1
            a2 <- a2 + cheatsN

    printfn $"%A{a}"
    printfn $"%A{a2}"

    (*
    printfn $"%A{(start, target)}"
    printfn $"%A{r}"
    *)



    0
