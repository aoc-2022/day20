open System.IO

let parse (s: string) = s |> int

let swap (a, b) = (b, a)

let input = File.ReadAllLines "/tmp/aoc/input" |> Seq.map parse |> Seq.toList

input |> List.map (printfn "%A")

printfn $"input {input |> List.min} {input |> List.max} {input.Length}"

type Ring(index: Map<int, int>,rev: Map<int,int> , size:int) =
   
    member this.Index = index
    member this.Rev = rev
    member this.getIndexOf (n: int) : int =
        index[n]
    member this.getAtIndex (i: int) : int =
        let i = if i < 1 then i + (size-1) else i
        let i = if i < 1 then i + (size-1) else i
        let i = if i > size then i % size + 1 else i
        let i = if i > size then i % size + 1 else i
        index |> Map.toList |> List.find (fun (_,n) -> i = n) |> fst 
        
    member this.Size = size 
    member this.Move(n: int) =
        let oldSize = (index.Count,rev.Count)
        let i = index[n]
        let old = i
        let index = index.Remove n
        let rev = rev.Remove i

        let index =
            index
            |> Map.toSeq
            |> Seq.map (fun (a, b) -> if (b > i) then (a, b - 1) else (a, b))
            |> Map.ofSeq
        let rev =
            rev
            |> Map.toSeq
            |> Seq.map (fun (a,b) -> if (a > i) then (a - 1,b) else (a,b))
            |> Map.ofSeq 

        let i = i + n
        let i = if i < 1 then i + (size-1) else i
        let i = if i < 1 then i + (size-1) else i
        let i = if i > size then i % size + 1 else i
        let i = if i > size then i % size + 1 else i

        let index =
            index
            |> Map.toSeq
            |> Seq.map (fun (a, b) -> if (b >= i) then (a, b + 1) else (a, b))
            |> Map.ofSeq
        let rev =
            rev
            |> Map.toSeq
            |> Seq.map (fun (a,b) -> if (a >= i) then (a + 1, b) else (a,b))
            |> Map.ofSeq
            
        let index = index.Add(n, i)
        let rev = rev.Add(i,n)
        let newSize = (index.Count,rev.Count)
        
        if oldSize <> newSize then
            printfn $"Lost something {i}"
            1 / 0
        else
            1
        
        // printfn $"move {n} @ {old} -> {i}"
        Ring(index,rev,size)

    override this.ToString() =
        let inums =
            index
            |> Map.toSeq
            |> Seq.map swap
            |> Seq.sort
            |> Seq.map fst 
            |> Seq.map (fun i -> $"{i}")
            |> String.concat " "

        let nums =
            index
            |> Map.toSeq
            |> Seq.map swap
            |> Seq.sort
            |> Seq.map snd 
            |> Seq.map (fun i -> $"{i}")
            |> String.concat " "

        $"Ring [{nums}] # {inums}]"

    static member init(nums: int list) =
        let index = nums |> List.indexed |> List.map swap |> Map.ofSeq
        let rev = nums |> List.indexed |> Map.ofSeq
        Ring(index,rev,nums.Length)


let ring = Ring.init input

let rec moveRot (ring: Ring) (nums: int list) =
    if nums.IsEmpty then
        ring
    else
        printfn $"Move: {nums.Head} @ {nums.Length}"
        // printfn $"M {nums.Head} {ring} "
        let ring = ring.Move nums.Head
        moveRot ring nums.Tail

let ringX = moveRot ring input

let pos0 = ringX.getIndexOf 0
printfn $"pos0 = {pos0}"

printfn $"ring = {ring}"

// printfn $"Final {ringX}"


let num1 = ringX.getAtIndex (pos0 + 1000)
let num2 = ringX.getAtIndex (pos0 + 2000)
let num3 = ringX.getAtIndex (pos0 + 3000)

let ans = num1 + num2 + num3 

printfn $"Final {ringX}"

printfn $"Final res = 0={pos0} @1000 = {num1} @2000 = {num2} @3000 = {num3}"

printfn $"ANSWER: {ans}"

// ring.getIndexOf -3 |> printfn "A = %A"