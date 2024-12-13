module aoc2024.day13

open System.Collections.Generic
open Microsoft.FSharp.Core
open aoc2024.util

let tryFn ax ay bx by px py ac bc =
    let d = Dictionary<int64 * int64, bool * int64 option>()
    let stack = Stack<int64 * int64>()
    stack.Push(ac, bc)

    let mutable result = (false, None)

    while stack.Count > 0 do
        let ac, bc = stack.Pop()

        if ((ax * ac + bx * bc) = px) && ((ay * ac + by * bc) = py) then
            result <- (true, Some(3L * ac + 1L * bc))
            stack.Clear()
        elif ((ax * ac + bx * bc) >= px) && ((ay * ac + by * bc) >= py) then
            ()
        else
            let nPresses = [ 1L ]

            for np in nPresses do
                let na = (ac + np, bc)
                let nb = (ac, bc + np)

                if not (d.ContainsKey na) then
                    d[na] <- (false, None)
                    stack.Push(na)

                if not (d.ContainsKey nb) then
                    d[nb] <- (false, None)
                    stack.Push(nb)

                let a = d[na]
                let b = d[nb]

                let valid = [ a; b ] |> Seq.where fst

                if not (valid |> Seq.isEmpty) then
                    result <- valid |> Seq.minBy snd
                    stack.Clear() // Exit the loop

    result

let solveLinearEquations (cfAX: int64) (cfAY: int64) (cfBX: int64) (cfBY: int64) (constantX: int64) (constantY: int64) =
    let determinant = cfAX * cfBY - cfAY * cfBX

    if determinant = 0 then
        None
    else
        let x = (constantX * cfBY - constantY * cfBX) / determinant
        let y = (cfAX * constantY - cfAY * constantX) / determinant
        Some(x, y)

let rec solve () =
    let io = aocIO
    let inp = io.getInput ()

    let w = inp |> Seq.chunkBySize 4 |> Seq.map (fun s -> s |> Seq.take 3 |> Seq.toList)

    let mutable a1 = 0L

    for conf in w do
        let a = conf[0] |> String.extractAllNumsUint64 |> List.map int64
        let b = conf[1] |> String.extractAllNumsUint64 |> List.map int64
        let p = conf[2] |> String.extractAllNumsUint64 |> List.map int64

        let ax, ay = (a[0], a[1])
        let bx, by = (b[0], b[1])
        let px, py = (p[0], p[1])

        let add = 10000000000000L
        let px, py = (px + add, py + add)


        let (f, s) = (solveLinearEquations ax ay bx by px py).Value

        if ((f * ax + s * bx) = px && (f * ay + s * by) = py) then
            a1 <- a1 + ((f * 3L) + s)
            printfn $"%A{(f, s)}"



        ()

    printfn $"%A{a1}"
    //io.submitAnswer 1 a1
    //io.submitAnswer 2 a2
    0
