module aoc2024.day07

open System
open System.Collections.Generic
open aoc2024.util

let test res ns =
    let q = Queue<bigint>()
    for n in ns do
        q.Enqueue n
        
    let results = List<bigint>()
        
    let rec l1 acc1 acc2 acc3 act1 act2 act3 (q: Queue<bigint>) =
        if q.Count = 0 then
            results.Add(acc1)
            results.Add(acc2)
            results.Add(acc3)
            ()
        else if (acc1 = (-123) && acc2 = (-123) && acc3 = (-123)) then
            let n1 = q.Dequeue()
            let n2 = q.Dequeue()
            let nq1 = Queue<bigint>()
            let nq2 = Queue<bigint>()
            let nq3 = Queue<bigint>()
            while (q.Count > 0) do
                let n = q.Dequeue()
                nq1.Enqueue(n)
                nq2.Enqueue(n)
                nq3.Enqueue(n)
            l1 (act1 n1 n2) (act2 n1 n2) (act3 n1 n2) act1 act2 act3 nq1
            l1 (act3 n1 n2) (act1 n1 n2) (act2 n1 n2) act1 act2 act3 nq2
            l1 (act2 n1 n2) (act3 n1 n2) (act1 n1 n2) act1 act2 act3 nq3
        else
            let n1 = q.Dequeue()
            let nq1 = Queue<bigint>()
            let nq2 = Queue<bigint>()
            let nq3 = Queue<bigint>()
            while (q.Count > 0) do
                let n = q.Dequeue()
                nq1.Enqueue(n)
                nq2.Enqueue(n)
                nq3.Enqueue(n)
            l1 (act1 acc1 n1) (act2 acc2 n1) (act3 acc3 n1) act1 act2 act3 nq1
            l1 (act3 acc1 n1) (act1 acc2 n1) (act2 acc3 n1) act1 act2 act3 nq2
            l1 (act2 acc1 n1) (act3 acc2 n1) (act1 acc3 n1) act1 act2 act3 nq3
        
 
    l1 (-123) (-123) (-123) (+) (*) (fun x y -> UInt64.Parse(x.ToString() + y .ToString())) q
    results |> Seq.tryFind(fun x -> x = res) |> Option.isSome

let solve() =
    let io = aocIO
    let inp = io.getInput()
    
    let mutable a2 = 0 |> bigint
    for l in inp  do
        let nums = l |> String.extractAllNumsU |> Seq.map (fun x -> x |> bigint) |> Seq.toArray
        let res = nums[0]
        let ns = nums[1..]
        &a2 += if (test res ns) then res else 0UL
        
    
    //io.submitAnswer 1 a1
    printfn $"%A{a2}"
    //io.submitAnswer 2 a2
    0
   