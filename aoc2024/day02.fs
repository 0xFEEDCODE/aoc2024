module aoc2024.day02
open aoc2024.util

let isSafe nums =
    let nums = nums |> Seq.toArray
    let ascending = nums[1] > nums[0]
    
    seq {1..(nums.Length-1)}
    |> Seq.forall
        (fun i ->
            let prev = nums[i-1]
            let curr = nums[i]
            let diff = curr - prev
            (abs diff > 0 && abs diff < 4 && ((diff > 0) = ascending)))
    
let solve() =
    
    let inp = aocIO.getInput()
    let ans1 =
        inp
        |> Seq.map String.extractAllNums
        |> Seq.where isSafe
        |> Seq.length
        
    let ans2 =
        inp
        |> Seq.map String.extractAllNums
        |> Seq.where (fun nums ->
            isSafe nums || ({-1..(nums.Length-1)} |> Seq.exists (fun i -> isSafe (Seq.append nums[..i] nums[(i+2)..] ))))
        |> Seq.length
        
    printfn $"%A{ans1}"
    printfn $"%A{ans2}"
