module aoc2024.day17

open System
open aoc2024.util
open aoc2024.util.String

type RegOrValue =
    | Reg of int64 ref
    | Value of int64

type Operation =
    | Adv
    | Bxl
    | Bst
    | Jnz
    | Bxc
    | Out
    | Bdv
    | Cdv

let mutable output = ""

type Mem() =
    let mutable a = 0L
    let mutable b = 0L
    let mutable c = 0L

    let dv (v: int64) =
        ((a |> float) / Math.Pow(2, v |> float)) |> int64 // CHECK IF TRUNCATED?

    member this.A
        with get () = a
        and set (v) = a <- v

    member this.B
        with get () = b
        and set (v) = b <- v

    member this.C
        with get () = c
        and set (v) = c <- v

    member this.Print() = printfn $"%A{(a, b, c)}"

    member this.Reset() =
        a <- 0L
        b <- 0L
        c <- 0L

    member this.ExecuteInstr (ip: int) (op: Operation) (v: RegOrValue) =
        let v =
            match v with
            | Value vv -> vv
            | Reg r -> r.Value

        match op with
        | Adv ->
            a <- dv v
            ip + 2
        | Bxl ->
            b <- (b ^^^ v)
            ip + 2
        | Bst ->
            b <- v % 8L
            ip + 2
        | Jnz -> if a <> 0 then (v |> int) else ip + 2
        | Bxc ->
            b <- (b ^^^ c)
            ip + 2
        | Out ->
            //printfn $"%A{(v, (a,b,c))}"
            if output = "" then
                output <- $"%d{(v % 8L)}"
            else
                output <- output + $",%d{(v % 8L)}"

            ip + 2
        | Bdv ->
            b <- dv v
            ip + 2
        | Cdv ->
            c <- dv v
            ip + 2

let mem = Mem()


let opcodeMap =
    Map.empty
        .Add(0L, Adv)
        .Add(1L, Bxl)
        .Add(2L, Bst)
        .Add(3L, Jnz)
        .Add(4L, Bxc)
        .Add(5L, Out)
        .Add(6L, Bdv)
        .Add(7L, Cdv)

let operandMap =
    Map.empty
        .Add(0L, Value 0L)
        .Add(1L, Value 1L)
        .Add(2L, Value 2L)
        .Add(3L, Value 3L)
        .Add(4L, Reg(ref mem.A))
        .Add(5L, Reg(ref mem.B))
        .Add(6L, Reg(ref mem.C))

let solve () =
    let io = aocIO
    let inp = io.getInput () |> Seq.toList

    let ma = inp[0] |> extractAllNums |> Seq.head
    let mb = inp[1] |> extractAllNums |> Seq.head
    let mc = inp[2] |> extractAllNums |> Seq.head

    let program = inp[4] |> extractAllNums |> Seq.map int64 |> Seq.toList
    mem.A <- ma
    mem.B <- mb
    mem.C <- mc

    printfn $"%A{(ma, mb, mc, program)}"

    let mutable ip = 0

    let desiredOut =
        program
        |> Seq.fold (fun acc x -> if acc <> "" then acc + "," + x.ToString() else x.ToString()) ""

    printfn $"%A{desiredOut}"

    let mutable outs = []

    let n = 265061364597300L

    let nextN = Int64.Parse(n.ToString() + "000")

    let mutable i = n

    while i < nextN do
        mem.Reset()
        mem.A <- i

        ip <- 0
        output <- ""

        while (ip < program.Length) do
            let operandMap =
                Map.empty
                    .Add(0L, Value 0L)
                    .Add(1L, Value 1L)
                    .Add(2L, Value 2L)
                    .Add(3L, Value 3L)
                    .Add(4L, Reg(ref mem.A))
                    .Add(5L, Reg(ref mem.B))
                    .Add(6L, Reg(ref mem.C))
                    .Add(7L, Value 7L)

            let (instr, op) = (program[ip], program[(ip) + 1])
            let (instr, op) = (opcodeMap[instr], operandMap[op])
            ip <- mem.ExecuteInstr ip instr op

        //printfn $"%A{(i, output)}"

        //Program: 2,4,1,7,7,5,1,7,4,6,0,3,5,5,3,0
        if (output.EndsWith "2,4,1,7,7,5,1,7,4,6,0,3,5,5,3,0" && output.Length = 31) then

            printfn $"%A{(i, output)}"
            printfn $"%A{output = desiredOut}"
            printfn $"%A{desiredOut}"
            exit (0)

        i <- i + 1L




    (*
        printfn $"IP"
        printfn $"%A{ip}"
        mem.Print()
        *)


    outs |> Seq.iter (printfn "%A")
    printfn "Not found"

    //io.submitAnswer 1 a1
    //io.submitAnswer 2 a2
    0
