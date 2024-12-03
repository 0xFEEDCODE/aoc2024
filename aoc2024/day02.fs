module aoc2024.day02

open aoc2024.util

let isSafe (n: int list) =
    let mutable is = true
    let n = n |> Seq.toArray

    let isIncreasing = n[0] < n[1]
    let mutable i = 1

    while (i < (n |> Seq.length)) do
        if is then
            if isIncreasing then
                if (n[i] < n[i - 1]) then
                    is <- false

            if not isIncreasing then
                if (n[i] > n[i - 1]) then
                    is <- false

            if (abs (n[i] - n[i - 1]) < 1 || abs (n[i] - n[i - 1]) > 3) then
                is <- false

        i <- i + 1

    is

let solve () =
    let inp = aocIO.getInput ()

    printfn $"%A{inp}"

    //let ans2 = inp |> Seq.sumBy(fun l -> (isSafe (l |> String.extractAllNums))?(1,0))

    let mutable s = 0

    for l in inp do
        let mutable nums = l |> String.extractAllNums |> Seq.toList
        let mutable is = isSafe nums

        if not is then
            for i in 0 .. (nums.Length - 1) do
                if not is then
                    let saved = int nums[i]
                    nums <- nums |> Seq.removeAt i |> Seq.toList

                    if i = 3 then
                        printfn $""

                    is <- isSafe nums
                    nums <- nums |> Seq.insertAt i saved |> Seq.toList

        printfn $"%A{is}"

        if is then
            s <- s + 1



    printfn $"%A{s}"

    //aocIO.submitAnswer 1 ans1
    //aocIO.submitAnswer 2 ans2

    0
