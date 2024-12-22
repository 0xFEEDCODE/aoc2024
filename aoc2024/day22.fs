module aoc2024.day22

open System
open System.Collections.Generic
open System.Linq
open aoc2024.util

let prune n = n % 16777216UL
let mix n1 n2 = n1 ^^^ n2

let getLD n = n % 10UL

let proc secretN =
    let mutable secretN = secretN

    let c1 = (secretN * 64UL)
    secretN <- secretN |> mix c1
    secretN <- secretN |> prune

    let c2 = (secretN / 32UL)
    secretN <- secretN |> mix c2
    secretN <- secretN |> prune

    let c3 = secretN * 2048UL
    secretN <- secretN |> mix c3
    secretN <- secretN |> prune

    secretN

type FourNums = int * int * int * int

let solve () =
    let io = aocIO
    let inp = io.getInput () |> Seq.map UInt64.Parse


    let res =
        inp
        |> Seq.map (fun x ->
            let mutable n = x

            for i in 0 .. 2000 - 1 do
                n <- proc n

            n)

    printfn $"%A{res |> Seq.sum}"

    let res =
        inp
        |> Seq.map (fun n ->
            Seq.init 2000 id
            |> Seq.scan
                (fun acc _ ->
                    let (prevLd, prevDiff, n) = acc
                    let prevN = n
                    let n = proc n
                    let ld = int (getLD n)

                    let diff =
                        if prevLd |> Option.isSome then
                            Some(ld - (prevLd.Value))
                        else
                            Some(ld - int (getLD prevN))

                    (Some(ld), diff, n))
                (None, None, n))


    let records = Dictionary<FourNums, int>()

    for b in res do
        let bi = b |> smapi |> Seq.toArray

        let fcons =
            bi
            |> Seq.skip 1
            |> Seq.take (bi.Length - 2)
            |> Seq.map (fun (idx, (_, diff, __)) -> (idx, diff.Value))
            |> Seq.windowed 4
            |> Seq.map (fun x ->
                let idx = x |> Seq.last |> fst
                let (ld, _, _) = snd bi[idx]
                (x |> Seq.map snd, ld.Value))
            |> Seq.toArray

        let localRecords = Dictionary<FourNums, int>()

        for nums, n in fcons do
            let nums = nums |> Seq.toArray
            let fourNums = FourNums(nums[0], nums[1], nums[2], nums[3])

            if (not (localRecords.ContainsKey(fourNums))) then
                localRecords[fourNums] <- n

        for r in localRecords do
            if (not (records.ContainsKey(r.Key))) then
                records[r.Key] <- 0

            records[r.Key] <- records[r.Key] + r.Value


    printfn $"%A{records.Values.Max()}"

    0
