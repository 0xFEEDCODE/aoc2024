module aoc2024.day19

open System.Collections.Generic
open aoc2024.util

let solve () =
    let io = aocIO
    let inp = io.getInput ()

    let patterns = (inp |> Seq.head).Split ',' |> Seq.map (_.Trim())
    let mutable designs = inp |> Seq.skip 2 |> Seq.toList

    let mutable n = 0UL

    let cached = Dictionary<string, uint64>()

    let canMake (target: string) =
        let patterns = patterns |> Seq.where target.Contains

        let rec loop (left: string) (right: string) =
            if right.Length = 0 then
                1UL
            else
                patterns
                |> Seq.where right.StartsWith
                |> Seq.sumBy (fun p ->
                    let newL = left + p
                    let newR = right.Substring(p.Length, right.Length - p.Length)

                    if not (cached.ContainsKey(newR)) then
                        cached[newR] <- loop newL newR

                    cached[newR])

        loop "" target


    (*
        patterns
        |> Seq.where (fun p -> p.Length <= d.Length && d.StartsWith p)
        |> Seq.sumBy (fun _ -> nPossibleWays d 0)
        *)

    let mutable i = 0UL

    for d in designs do
        let r = canMake d
        n <- n + r
        printfn $"%A{i}"


        i <- i + 1UL

    printfn $"%A{n}"

    0
