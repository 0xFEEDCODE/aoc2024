module aoc2024.helper

open System
open System.Collections.Generic
open System.Diagnostics
open System.IO
open System.Reflection
open System.Text
open System.Text.RegularExpressions
open Microsoft.Extensions.Configuration
open System.Net.Http

open System.Threading

let smapi x = x |> Seq.mapi (fun i x -> (i, x))

let lw (a, b) = (lazy a, lazy b)
let (?) is_true (a, b) = if is_true then a else b

let ($) is_true (a: Lazy<'a>, b: Lazy<'a>) =
    if is_true then a.Force() else b.Force()

let permString (data: string) =
    let swap (i: int) (j: int) (data: StringBuilder) =
        let temp = data[i]
        data[i] <- data[j]
        data[j] <- temp

    let mutable acc = List.empty

    let rec perm k (dsb: StringBuilder) =
        if k = 1 then
            acc <- dsb.ToString() :: acc
        else
            let sb_copy = StringBuilder().Append(dsb)
            perm (k - 1) sb_copy

            for i = 0 to (k - 1) do
                swap ((k % 2 = 0)?(i, 0)) (k - 1) dsb

                perm (k - 1) dsb

    perm data.Length (StringBuilder(data))
    acc

let perm (data: 'a seq) =
    let swap (i: int) (j: int) (data: 'a array) =
        let temp = data[i]
        data[i] <- data[j]
        data[j] <- temp

    let mutable acc = List.empty

    let rec perm k data =
        if k = 1 then
            acc <- data :: acc
        else
            perm (k - 1) (data |> Seq.toArray)

            for i = 0 to (k - 1) do
                swap ((k % 2 = 0)?(i, 0)) (k - 1) data

                perm (k - 1) data

    perm (data |> Seq.length) (data |> Seq.toArray)
    acc |> List.toSeq

type Point2D =
    struct
        val x: int
        val y: int
        new(x: int, y: int) = { x = x; y = y }

        member this.GetManhattanDistance(p: Point2D) = abs (this.x - p.x) + abs (this.y - p.y)
        override this.ToString() = $"[{this.x};{this.y}]"
    end

type Point3D =
    struct
        val x: int
        val y: int
        val z: int
        new(x: int, y: int, z: int) = { x = x; y = y; z = z }
        override this.ToString() = $"[{this.x};{this.y};{this.z}]"
    end

module aocIO =
    type Util() =
        let httpClient = new HttpClient()

        let sessionToken =
            let secrets =
                ConfigurationBuilder().AddUserSecrets(Assembly.GetExecutingAssembly()).Build()

            (secrets.AsEnumerable() |> Seq.find (fun s -> s.Key = "tokenAOC")).Value

        let getCallerModuleName () =
            let st = StackTrace()
            let mutable i = 0

            while (st.GetFrame(i).GetMethod().ReflectedType.Name = "Util") do
                i <- i + 1

            st.GetFrame(i).GetMethod().ReflectedType.Name

        let getPathToInputFile moduleName =
            Path.Combine(Directory.GetParent(Environment.CurrentDirectory).Parent.Parent.FullName, $"{moduleName}.txt")

        member this.readInput() =
            let moduleName = getCallerModuleName ()
            File.ReadLines(getPathToInputFile moduleName)

        member this.getInput(?overrideIfExists: bool) =
            let overrideIfExists = defaultArg overrideIfExists false

            let moduleName = getCallerModuleName ()
            let pathToInputFile = getPathToInputFile moduleName

            let mutable shouldDownloadInput = true

            if (not overrideIfExists) then
                if (File.Exists pathToInputFile) then
                    shouldDownloadInput <- false

            if shouldDownloadInput then
                printfn "Downloading"

                let urlDayPart =
                    if moduleName.StartsWith "day0" then
                        moduleName[4..]
                    else
                        moduleName[3..]

                let url = $"https://adventofcode.com/2024/day/{urlDayPart}/input"
                let request = new HttpRequestMessage(HttpMethod.Get, url)
                request.Headers.Add("Cookie", $"session={sessionToken}")
                let cts = new CancellationTokenSource()
                let response = httpClient.Send request

                if (not response.IsSuccessStatusCode) then
                    failwith $"Could not get input, http error - {response.StatusCode}"

                let content = response.Content.ReadAsStringAsync(cts.Token)
                content |> Async.AwaitTask |> ignore
                File.WriteAllText(pathToInputFile, content.Result)

            this.readInput ()

        member this.submitAnswer part answer =
            let moduleName = getCallerModuleName ()

            let urlDayPart =
                if moduleName.StartsWith "day0" then
                    moduleName[4..]
                else
                    moduleName[3..]

            let url = $"https://adventofcode.com/2024/day/{urlDayPart}/answer"
            let request = new HttpRequestMessage(HttpMethod.Post, url)
            request.Headers.Add("Cookie", $"session={sessionToken}")

            let payload = List<KeyValuePair<string, string>>()
            payload.Add(KeyValuePair<string, string>("level", part.ToString()))
            payload.Add(KeyValuePair<string, string>("answer", answer.ToString()))
            request.Content <- new FormUrlEncodedContent(payload)

            let cts = new CancellationTokenSource()
            let response = httpClient.Send request

            if (not response.IsSuccessStatusCode) then
                failwith $"Could submit answer, http error - {response.StatusCode}"

            let content = response.Content.ReadAsStringAsync(cts.Token)
            content |> Async.AwaitTask |> ignore

            let html = content.Result
            let pattern = "(?i)(?s)<article><p>(?<content>.*?)</p></article>"
            let m = Regex.Match(html, pattern)

            if m.Success then
                let content = m.Groups.["content"].Value
                printfn $"%A{content}"
                content
            else
                failwith "wtf"


module Grid =
    let createGrid<'a> nRows nCols =
        Array.zeroCreate<Array> nRows
        |> Seq.map (fun _ -> Array.zeroCreate<'a> nCols)
        |> Seq.toArray

    let createGridDV<'a> nRows nCols (initValue: 'a) =
        Array.zeroCreate<Array> nRows
        |> Seq.map (fun _ -> Array.create<'a> nCols initValue)
        |> Seq.toArray

    let getNeighbours (point: Point2D) =
        [ (-1, 0); (1, 0); (0, -1); (0, 1) ]
        |> Seq.map (fun (x, y) -> Point2D(point.x + x, point.y + y))
        |> Seq.toArray

    let getNeighboursDiag (point: Point2D) =
        [ (-1, 0); (1, 0); (0, -1); (0, 1); (1, 1); (-1, 1); (1, -1); (-1, -1) ]
        |> Seq.map (fun (x, y) -> Point2D(point.x + x, point.y + y))
        |> Seq.toArray

let util = aocIO.Util()
