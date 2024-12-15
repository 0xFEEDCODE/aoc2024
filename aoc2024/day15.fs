module aoc2024.day15

open System.Text
open System.Threading
open Microsoft.FSharp.Core
open aoc2024.util

(*

type P2D(x, y) as self =
    let mutable x = x
    let mutable y = y

    member this.X
        with get () = x
        and set (v) = x <- v

    member this.Y
        with get () = y
        and set (v) = y <- v

    member this.add(b: P2D) =
        x <- this.X + b.X
        y <- this.Y + b.Y

    static member (+)(a: P2D, b: P2D) = P2D(a.X + b.X, a.Y + b.Y)

    override this.Equals(obj) =
        match obj with
        | :? P2D as p -> p.X = this.X && p.Y = this.Y
        | _ -> false


    override this.ToString() = $"({this.X}, {this.Y})"
    
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
    let mutable robot = P2D(0, 0)


    let h = mapH
    let w = 9

    let pr () =
        for y in 0..h do
            for x in 0..w do
                let p = P2D(x, y)

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
                boundaries <- boundaries @ [ P2D(x, y) ]

            if ch = 'O' then
                boxes <- boxes @ [ P2D(x, y) ]

            if ch = '@' then
                robot <- P2D(x, y)

            x <- x + 1

        y <- y + 1

    printfn $"%A{robot}"
    printfn $"%A{boundaries.Length}"
    printfn $"%A{boxes.Length}"

    let dir =
        Map.empty
            .Add('^', P2D(0, -1))
            .Add('v', P2D(0, 1))
            .Add('<', P2D(-1, 0))
            .Add('>', P2D(1, 0))

    let rec canMove p d =
        if (boundaries |> List.contains p) then false
        else if (boxes |> List.contains p) then (canMove (p + d) d)
        else true

    for move in moves do
        let newRobotPos = robot + dir[move]

        if canMove newRobotPos dir[move] then
            robot.add (dir[move])

            let mutable sp =
                boxes
                |> List.where (fun p -> p = newRobotPos || boxes |> List.where (fun y -> p = y) |> List.length > 1)

            while (sp.Length > 0) do
                sp.Head.add dir[move]

                sp <-
                    boxes
                    |> List.where (fun p -> p = newRobotPos || boxes |> List.where (fun y -> p = y) |> List.length > 1)


    //pr ()


    let mutable a1 = bigint.Zero
    let mutable a2 = 0

    for b in boxes do
        a1 <- a1 + ((b.Y |> bigint) * (bigint 100) + (b.X |> bigint))

    printfn $"%A{a1}"


    io.submitAnswer 1 a1
    //io.submitAnswer 2 a2
    0
    *)


type P2D(x, y) as self =
    let mutable x = x
    let mutable y = y
    let mutable id = 0

    member this.X
        with get () = x
        and set (v) = x <- v

    member this.Y
        with get () = y
        and set (v) = y <- v

    member this.Id
        with get () = id
        and set (v) = id <- v

    member this.add(b: P2D) =
        x <- this.X + b.X
        y <- this.Y + b.Y


    static member (+)(a: P2D, b: P2D) =
        let np = P2D(a.X + b.X, a.Y + b.Y)
        np.Id <- a.Id
        np

    member this.boxRightSide = this + P2D(1, 0)

    override this.Equals(obj) =
        match obj with
        | :? P2D as p -> p.X = this.X && p.Y = this.Y
        | _ -> false


    override this.ToString() = $"({this.X}, {this.Y})"

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
    let mutable robot = P2D(0, 0)

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

    //nmap <- map |> Seq.toList

    for line in nmap do
        let mutable x = 0

        for ch in line do
            if ch = '#' then
                boundaries <- boundaries @ [ P2D(x, y) ]

            if ch = '[' then
                let b = P2D(x, y)
                b.Id <- id
                id <- id + 1
                boxes <- boxes @ [ b ]

            if ch = '@' then
                robot <- P2D(x, y)

            x <- x + 1

        y <- y + 1


    let prF () =
        for y in 0..mapH do
            let mutable x = 0

            while (x < nmap[0].Length) do
                let p = P2D(x, y)

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

    let pr () =
        for y in (robot.Y) - 5 .. (robot.Y) + 5 do
            let mutable x = robot.X - 10

            while (x < robot.X + 10) do
                let p = P2D(x, y)

                if boundaries |> List.contains p then
                    printf "#"
                else if boxes |> List.contains p then
                    x <- x + 1
                    printf "[]"
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
            .Add('^', P2D(0, -1))
            .Add('v', P2D(0, 1))
            .Add('<', P2D(-1, 0))
            .Add('>', P2D(1, 0))


    let hitsBoundary p isR =
        (boundaries
         |> List.tryFind (fun boundary ->
             if isR then
                 boundary = p
             else
                 (boundary = p || boundary = p.boxRightSide))
         |> Option.isSome)

    let boxCollisionRobot (robot: P2D) (box: P2D) (d: P2D) = robot = box.boxRightSide || robot = box

    let boxCollisionBox (b1: P2D) (b2: P2D) (d: P2D) =
        (b1 = b2 || (b1 = b2.boxRightSide || (b1.boxRightSide = b2)))


    let getBoxAtPosition p isR d =
        boxes
        |> List.where (fun b ->
            if isR then
                boxCollisionRobot p b d
            else
                p.Id <> b.Id && boxCollisionBox p b d)

    let rec canMove p d isRobot =
        if hitsBoundary p isRobot then
            false
        else
            let bx = getBoxAtPosition p isRobot d

            if ((bx |> Seq.length) > 0) then
                bx |> Seq.forall (fun nb -> (canMove (nb + d) d false))
            else
                true


    let mutable debug = false

    let mutable stepsLeft = moves.Length
    let mutable i = 0

    prF ()

    for move in moves do
        let newRobotPos = robot + dir[move]


        printfn $"%A{(i, stepsLeft, move)}"

        i <- + 1

        (*
        if stepsLeft <= 16291 + 1 then
            prF ()
            *)

        if canMove newRobotPos dir[move] true then
            robot.add (dir[move])

            let mutable sp =
                boxes
                |> List.where (fun box -> box = newRobotPos || box.boxRightSide = newRobotPos)

            let mutable fail = false


            while (sp.Length > 0) do
                (*
                if not (sp |> Seq.forall (fun x -> canMove x dir[move] true)) then
                    prF ()
                    fail <- true
                    *)

                let prev = sp
                prev |> Seq.iter (fun x -> x.add dir[move])

                sp <-
                    boxes
                    |> List.where (fun b1 ->
                        prev
                        |> Seq.tryFind (fun b2 -> b1.Id <> b2.Id && (b1 = b2 || b1 = b2.boxRightSide || b1.boxRightSide = b2))
                        |> Option.isSome)

        (*
            if fail then
                prF ()
                failwith "wtf"
                *)


        if debug then
            pr ()
            Thread.Sleep(1)
            ()

        stepsLeft <- stepsLeft - 1

    let mutable a1 = bigint.Zero
    let mutable a2 = 0

    for b in boxes do
        a1 <- a1 + ((b.Y |> bigint) * (bigint 100) + (b.X |> bigint))

    prF ()
    printfn $"%A{a1}"


    //io.submitAnswer 1 a1
    //io.submitAnswer 2 a1
    0
