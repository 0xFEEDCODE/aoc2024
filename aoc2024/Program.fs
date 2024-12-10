open System.Collections
open System.Diagnostics
open System.Linq
open aoc2024
open aoc2024.util.Seq
open util

let sw = Stopwatch()

sw.Start()
(*
day01.solve()
day02.solve()
day03.solve()
day04.solve()
day05.solve()
day06.solve()
day07.solve()
day08.solve()
*)
day10.solve()

sw.Stop()
printfn $"Time taken - %A{sw.Elapsed}"
