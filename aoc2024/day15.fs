module aoc2024.day15

open System.Text
open Microsoft.FSharp.Core
open aoc2024.util

type Point(x, y) =
    let mutable x = x
    let mutable y = y
    let mutable id = 0

    member this.X
        with get () = x
        and set v = x <- v

    member this.Y
        with get () = y
        and set v = y <- v

    member this.Id
        with get () = id
        and set v = id <- v

    member this.Add(b: Point) =
        x <- this.X + b.X
        y <- this.Y + b.Y

    static member (+)(a: Point, b: Point) =
        let np = Point(a.X + b.X, a.Y + b.Y)
        np.Id <- a.Id
        np

    member this.boxRightSide = this + Point(1, 0)

    override this.Equals(obj) =
        match obj with
        | :? Point as p -> p.X = this.X && p.Y = this.Y
        | _ -> false
        
let solvept1 () =
    let io = aocIO
    let inp = io.getInput ()

    let mapH = 50

    let map = inp |> Seq.take mapH
    let moves = inp |> Seq.skip (mapH + 1) |> Seq.fold (fun acc x -> acc + x) ""

    printfn $"%A{map |> Seq.last}"
    printfn $"%A{moves |> Seq.head}"
    printfn $""

    let mutable boundaries = List.empty
    let mutable boxes = List.empty
    let mutable robot = Point(0, 0)


    let h = mapH
    let w = 9

    let pr () =
        for y in 0..h do
            for x in 0..w do
                let p = Point(x, y)

                if boundaries |> List.contains p then printf "#"
                else if boxes |> List.contains p then printf "@"
                else if robot = p then printf "R"
                else printf "."

            printfn $""

    let mutable y = 0

    for line in map do
        let mutable x = 0

        for ch in line do
            if ch = '#' then
                boundaries <- boundaries @ [ Point(x, y) ]

            if ch = 'O' then
                boxes <- boxes @ [ Point(x, y) ]

            if ch = '@' then
                robot <- Point(x, y)

            x <- x + 1

        y <- y + 1

    printfn $"%A{robot}"
    printfn $"%A{boundaries.Length}"
    printfn $"%A{boxes.Length}"

    let dir =
        Map.empty
            .Add('^', Point(0, -1))
            .Add('v', Point(0, 1))
            .Add('<', Point(-1, 0))
            .Add('>', Point(1, 0))

    let rec canMove p d =
        if (boundaries |> List.contains p) then false
        else if (boxes |> List.contains p) then (canMove (p + d) d)
        else true

    for move in moves do
        let newRobotPos = robot + dir[move]

        if canMove newRobotPos dir[move] then
            robot.Add (dir[move])

            let mutable sp =
                boxes
                |> List.where (fun p -> p = newRobotPos || boxes |> List.where (fun y -> p = y) |> List.length > 1)

            while (sp.Length > 0) do
                sp.Head.Add dir[move]

                sp <-
                    boxes
                    |> List.where (fun p -> p = newRobotPos || boxes |> List.where (fun y -> p = y) |> List.length > 1)



    let mutable a1 = bigint.Zero
    let mutable a2 = 0

    for b in boxes do
        a1 <- a1 + ((b.Y |> bigint) * (bigint 100) + (b.X |> bigint))

    printfn $"%A{a1}"

    io.submitAnswer 1 a1
    //io.submitAnswer 2 a2
    0

let solve () =
    let io = aocIO
    let inp = io.getInput ()

    let mapH = 50

    let map = inp |> Seq.take mapH
    let moves = inp |> Seq.skip (mapH + 1) |> Seq.fold (fun acc x -> acc + x) ""

    printfn $"%A{map |> Seq.last}"
    printfn $"%A{moves |> Seq.head}"
    printfn $""

    let mutable boundaries = List.empty
    let mutable boxes = List.empty
    let mutable robot = Point(0, 0)

    let mutable nmap = List.empty

    for r in map do
        let mutable newRow = StringBuilder()

        for ch in r do
            if ch = '#' then
                newRow <- newRow.Append("##")
            else if ch = 'O' then
                newRow <- newRow.Append("[]")
            else if ch = '.' then
                newRow <- newRow.Append("..")
            else if ch = '@' then
                newRow <- newRow.Append("@.")

        nmap <- nmap @ [ newRow.ToString() ]

    let mutable y = 0

    let mutable id = 1

    for line in nmap do
        let mutable x = 0

        for ch in line do
            if ch = '#' then
                boundaries <- boundaries @ [ Point(x, y) ]

            if ch = '[' then
                let b = Point(x, y)
                b.Id <- id
                id <- id + 1
                boxes <- boxes @ [ b ]

            if ch = '@' then
                robot <- Point(x, y)

            x <- x + 1

        y <- y + 1


    let prF () =
        for y in 0..mapH do
            let mutable x = 0

            while (x < nmap[0].Length) do
                let p = Point(x, y)

                if boundaries |> List.contains p then
                    printf "#"
                else if boxes |> List.contains p then
                    if robot = p then printf "@]"
                    else if robot = p.boxRightSide then printf "[@"
                    else printf "[]"

                    x <- x + 1
                else if robot = p then
                    printf "@"
                else
                    printf "."

                x <- x + 1

            printfn $""

    printfn $"%A{robot}"
    printfn $"%A{boundaries.Length}"
    printfn $"%A{boxes.Length}"

    let dir =
        Map.empty
            .Add('^', Point(0, -1))
            .Add('v', Point(0, 1))
            .Add('<', Point(-1, 0))
            .Add('>', Point(1, 0))

    let hitsBoundary p isR =
        (boundaries
         |> List.tryFind (fun boundary ->
             if isR then
                 boundary = p
             else
                 (boundary = p || boundary = p.boxRightSide))
         |> Option.isSome)

    let boxCollisionRobot (robot: Point) (box: Point) = robot = box.boxRightSide || robot = box

    let boxCollisionBox (b1: Point) (b2: Point) =
        (b1 = b2 || (b1 = b2.boxRightSide || (b1.boxRightSide = b2)))

    let tryGetBoxAtPos pos isRobot =
        boxes
        |> List.where (fun box ->
            if isRobot then
                boxCollisionRobot pos box
            else
                (pos.Id <> box.Id) && boxCollisionBox pos box)

    let rec canMove pos dir isRobot =
        if hitsBoundary pos isRobot then
            false
        else
            let boxes = tryGetBoxAtPos pos isRobot

            if ((boxes |> Seq.length) > 0) then
                boxes |> Seq.forall (fun box -> (canMove (box + dir) dir false))
            else
                true

    let mutable stepsLeft = moves.Length
    let mutable i = 0

    for move in moves do
        let newRobotPos = robot + dir[move]

        i <- + 1

        if canMove newRobotPos dir[move] true then
            robot.Add(dir[move])

            let mutable boxesInFront =
                boxes
                |> List.where (fun box -> box = newRobotPos || box.boxRightSide = newRobotPos)


            while (boxesInFront.Length > 0) do
                let previouslyMovedBoxes = boxesInFront
                previouslyMovedBoxes |> Seq.iter (fun box -> box.Add dir[move])

                boxesInFront <-
                    boxes
                    |> List.where (fun b1 ->
                        previouslyMovedBoxes
                        |> Seq.tryFind (fun b2 -> (b1.Id <> b2.Id) && (b1 = b2 || b1 = b2.boxRightSide || b1.boxRightSide = b2))
                        |> Option.isSome)

        stepsLeft <- stepsLeft - 1

    let mutable a1 = bigint.Zero
    let mutable a2 = bigint.Zero

    for b in boxes do
        a2 <- a2 + ((b.Y |> bigint) * (bigint 101) + (b.X |> bigint))

    prF ()
    printfn $"%A{a2}"


    //io.submitAnswer 1 a1
    //io.submitAnswer 2 a1
    0
