open System.Diagnostics
open aoc2024

let sw = Stopwatch()

sw.Start()
(*
day01.solve()
day02.solve()
day03.solve()
day04.solve()
*)
sw.Stop()
day05.solve()

printfn $"Time taken - %A{sw.Elapsed}"
