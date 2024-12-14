module aoc2024.day14

open System
open aoc2024.util

(*
type Robot =
    struct
        val P: Point2D
        val V: Point2D
        new(p: Point2D, v: Point2D) = { P = p; V = v }
    end

    member this.move() = Robot(this.P + this.V, this.V)
    *)

// make robot that is class instead of struct and is mutable
type Robot() =
    let mutable p = Point2D(0, 0)
    let mutable v = Point2D(0, 0)

    member this.P
        with get () = p
        and set (sv) = p <- sv

    member this.V
        with get () = v
        and set (sv) = v <- sv

    member this.move boundX boundY =
        let np = p + v
        let mutable (nx, ny) = (np.x, np.y)

        if np.x < 0 then
            nx <- boundX - abs np.x + 1

        if np.y < 0 then
            ny <- boundY - abs np.y + 1

        if np.x > boundX then
            nx <- np.x - boundX - 1

        if np.y > boundY then
            ny <- np.y - boundY - 1

        this.P <- Point2D(nx, ny)

let mkRb p v =
    let rb = Robot()
    rb.P <- p
    rb.V <- v
    rb

let solve () =
    let io = aocIO
    let inp = io.getInput ()
    let mutable a1 = 0
    let mutable a2 = 0

    let mutable rb = []

    let boundY = (103) - 1
    let boundX = (101) - 1


    for l in inp do
        let n = l |> String.extractAllNums
        let p = Point2D(n[0], n[1])
        let v = Point2D(n[2], n[3])
        rb <- rb @ [ mkRb p v ]

    (*
    printfn $"%A{rb.Length}"
    let rb = [ mkRb (Point2D(2, 4)) (Point2D(2, -3)) ]
    *)

    let pr () =
        let rbt = rb |> List.map (fun x -> Point2D(x.P.x, x.P.y))

        for y in 0..boundY do
            for x in 0..boundX do
                let p = Point2D(x, y)

                if (rbt |> List.contains p) then
                    printf $"X"
                else
                    printf $"."

            printfn ""

        printfn ""



    let mutable t = true

    let mutable i = 0
    let mutable m = Int32.MinValue

    while t do
        for r in rb do
            r.move boundX boundY

        i <- i + 1
        let c = rb |> List.map (_.P)

        let nCount =
            c
            |> Seq.where (fun p ->
                p
                |> Grid.getAllNeighbours
                |> Seq.tryFind (fun n -> c |> List.contains n)
                |> Option.isSome)
            |> Seq.length

        if nCount > m then
            m <- nCount
            printfn $"%A{(i, nCount)}"
    (*
        then
            t <- false
            pr ()
            printfn $"%A{i}"
            *)



    //pr ()


    (*
    for i in 0..100 do
        for r in rb do
            r.move ()

    for r in rb do
        printfn $"%A{(r.P.x % W, r.P.y % H)}"
        *)
    let mp = Point2D(boundX / 2, boundY / 2)
    let rbt = rb |> List.map (fun x -> Point2D(x.P.x, x.P.y))


    let mutable s1 = 0

    for y in 0 .. mp.y - 1 do
        for x in 0 .. mp.x - 1 do
            let p = Point2D(x, y)
            s1 <- s1 + (rbt |> List.where (fun x -> x = p) |> Seq.length)

    let mutable s2 = 0

    for y in mp.y + 1 .. boundY + 1 do
        for x in mp.x + 1 .. boundX + 1 do
            let p = Point2D(x, y)
            s2 <- s2 + (rbt |> List.where (fun x -> x = p) |> Seq.length)


    let mutable s3 = 0

    for y in mp.y + 1 .. boundY + 1 do
        for x in 0 .. mp.x - 1 do
            let p = Point2D(x, y)
            s3 <- s3 + (rbt |> List.where (fun x -> x = p) |> Seq.length)

    let mutable s4 = 0

    for y in 0 .. mp.y - 1 do
        for x in mp.x + 1 .. boundX + 1 do
            let p = Point2D(x, y)
            s4 <- s4 + (rbt |> List.where (fun x -> x = p) |> Seq.length)

    printfn $"%A{(s1 * s2 * s3 * s4)}"

    ()









//io.submitAnswer 1 a1
//io.submitAnswer 2 a2
