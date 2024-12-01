module aoc2024.day01

open aoc2024.helper

let solve () =
    let mutable l1 = List.empty
    let mutable l2 = List.empty

    aocIO.getInput ()
    |> Seq.iter (fun x ->
        let spl = (x.Split "   ")
        l1 <- l1 @ [ int spl[0] ]
        l2 <- l2 @ [ int spl[1] ])

    l1 <- List.sortDescending l1 |> Seq.rev |> Seq.toList
    l2 <- List.sortDescending l2 |> Seq.rev |> Seq.toList

    let z = l1 |> Seq.zip l2

    let ans1 = z |> Seq.sumBy (fun x -> abs (fst x - snd x))

    let ans2 =
        l2 |> Seq.sumBy (fun x -> x * (l1 |> Seq.where (fun y -> x = y) |> Seq.length))

    printfn $"%A{Grid.getNeighboursDiag (Point2D(5, 3))}"
    printfn $"%A{ans1}"
    printfn $"%A{ans2}"

    (*
    util.submitAnswer 1 ans1
    util.submitAnswer 2 ans2
    *)

    0
