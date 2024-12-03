module aoc2024.day33

open System.Collections.Generic
open aoc2024.util
open System


type Line =
    { s: Point2D; e: Point2D }
    member this.intersects (l: Line) =
        let x1 = this.s.x
        let y1 = this.s.y
        let x2 = this.e.x
        let y2 = this.e.y
        let x3 = l.s.x
        let y3 = l.s.y
        let x4 = l.e.x
        let y4 = l.e.y

        let d = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
        if d = 0 then
            false
        else
            let a = x1 * y2 - y1 * x2
            let b = x3 * y4 - y3 * x4

            let x = (a * (x3 - x4) - (x1 - x2) * b) / d
            let y = (a * (y3 - y4) - (y1 - y2) * b) / d

            (x >= min x1 x2 && x <= max x1 x2 && y >= min y1 y2 && y <= max y1 y2 &&
             x >= min x3 x4 && x <= max x3 x4 && y >= min y3 y4 && y <= max y3 y4)
            
    member this.interesctingPoint l =
        let x1 = this.s.x
        let y1 = this.s.y
        let x2 = this.e.x
        let y2 = this.e.y
        let x3 = l.s.x
        let y3 = l.s.y
        let x4 = l.e.x
        let y4 = l.e.y

        let d = (x1 - x2) * (y3 - y4) - (y1 - y2) * (x3 - x4)
        if d = 0 then
            failwith "Lines are parallel"
        else
            let a = x1 * y2 - y1 * x2
            let b = x3 * y4 - y3 * x4

            let x = (a * (x3 - x4) - (x1 - x2) * b) / d
            let y = (a * (y3 - y4) - (y1 - y2) * b) / d

            Point2D(x, y)


let solve () =

    let path = List<Line>()

    let mutable currPos = Point2D(0, 0)
    let inp = (new aocIO 2019).getInput ()
    
    let makeLine = fun (p1: Point2D) (p2: Point2D) -> { s = p1; e = p2 }

    let path =
        (inp |> Seq.head).Split ','
        |> Seq.map (fun x ->
            let v = (x |> Seq.tail |> String.Concat) |> int

            match (x |> Seq.head) with
            | 'U' -> Point2D(v * -1, 0)
            | 'D' -> Point2D(v, 0)
            | 'L' -> Point2D(0, v * - 1)
            | 'R' -> Point2D(0, v)
            | _ -> failwith "wtf")
        |> Seq.scan(fun (cp, l) p -> ((cp + p), Some({s = cp; e = (cp+p)}))) (Point2D(0, 0), None)
        
    let lines = path |> Seq.map snd |> Seq.skip 1 |> Seq.toArray
    for i in 0..(lines |> Seq.length)-1 do
        for j in 0..(lines |> Seq.length)-1 do
            if(i <> j) then
                let l1 = lines[i].Value
                let l2 = lines[j].Value
                if(l1.intersects l2) then
                    printfn $"%A{(l1.interesctingPoint l2)}"
        
    0
    
