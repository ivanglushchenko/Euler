module Helpers

open System
open System.Collections

let stopwatch = new System.Diagnostics.Stopwatch()
let swStart () = stopwatch.Start()
let swStop () = 
    stopwatch.Stop()
    stopwatch.ElapsedMilliseconds

let getNextPrimesRange rangeFrom rangeTo primes =
    let (rangeFrom, primes) = if rangeFrom < 3L then (3L, [ 2L ]) else ((if rangeFrom % 2L = 0L then rangeFrom + 1L else rangeFrom), primes)
    let candidates = 
        [|  for x in rangeFrom..2L..(rangeTo) do
                if primes |> List.forall (fun prime -> x % prime <> 0L) then yield x |]
    let rec sieve all primes =
        if Array.isEmpty all then primes
        else 
            let nextPrime = all.[0]
            sieve (all |> Array.filter (fun n -> n % nextPrime <> 0L)) (nextPrime :: primes)
    sieve candidates primes

let getPrimes target f =
    getNextPrimesRange 3L target [2L] |> List.filter f

let getAllPrimes target = getPrimes target (fun n -> true)

let rec inline gcd (a: ^a) (b: ^a) : ^a = 
    let zero: ^a = LanguagePrimitives.GenericZero
    let rec compute (a: ^a) (b: ^a) = if b = zero then abs a else compute b ((a % b): ^a)
    compute a b

let getFactors target =
    let targetsr = int64(sqrt (float target))
    getPrimes targetsr (fun n -> target % (int64(n)) = 0L)

let toInt c = System.Int32.Parse(c.ToString())

let getLines s = System.IO.File.ReadAllLines s

let loadMatrixSep fn (c: char) = 
    let toArray (s: string) = 
        s.Split c
            |> Array.filter (fun t -> System.String.IsNullOrWhiteSpace t = false) 
            |> Array.map (fun t -> System.Int64.Parse t)
    getLines fn |> Array.map toArray

let loadMatrix fn = loadMatrixSep fn ' '

let pow n p = int(System.Math.Pow(float(n), float(p)))

let pow64 n p = int64(System.Math.Pow(float(n), float(p)))

let getPrimeDivisors n primes =
    let mutable remainder = n
    let mutable primeIndex = 0
    let mutable divisors = []
    while remainder > 1L && primeIndex < (primes |> Array.length) && primes.[primeIndex] <= n do
        let prime = primes.[primeIndex]
        let mutable primeDegree = 0L
        while remainder % prime = 0L do
            primeDegree <- primeDegree + 1L
            remainder <- remainder / prime
        if primeDegree > 0L then
            divisors <- (prime, primeDegree) :: divisors
        primeIndex <- primeIndex + 1
    divisors

let getProperDivisors list = 
    let rec loop list = 
        match list with
        | (a, b) :: hd :: tl ->
            let divisors = [ for x in 0L..b -> (if x = 0L then 1L else (x * a)) ]
            let rest = loop (hd :: tl)
            seq { for x in divisors do
                    for y in rest do
                        yield x * y } |> Seq.toList
        | (a, b) :: [] -> [ for x in 0L..b -> int64(System.Math.Pow(float(a), float(x))) ]
        | _            -> []
    loop list

let getProperDivisorsSum n primes = 
    let t = 
        getPrimeDivisors n primes 
        |> List.map (fun (p, a) -> (pow64 p (a + 1L) - 1L) / (p - 1L))
        |> List.fold (fun acc t -> acc * t) 1L
    t - n

let getDigits n = 
    let rec loop n acc =
        let (d, r) = (n / 10, n % 10)
        if d = 0 then r :: acc
        else loop d (r :: acc)
    loop n []

let getDigits64 n = 
    let rec loop n acc =
        let (d, r) = (n / 10L, n % 10L)
        if d = 0L then r :: acc
        else loop d (r :: acc)
    loop n []

let loadPrimes () =
    if System.IO.File.Exists "primes.txt" then
        System.IO.File.ReadAllLines("primes.txt") |> Array.map (fun l -> System.Int64.Parse l)
    else
        [| 2L |]

let calcPrimes nextNPrimes =
    let existingPrimes = loadPrimes () |> Array.sortBy (fun t -> -t) |> Array.toList
    let maxPrime = existingPrimes |> List.max
    let morePrimes = getNextPrimesRange maxPrime (maxPrime + nextNPrimes) existingPrimes |> List.sort

    use sw = new System.IO.StreamWriter("primes.txt", false)
    for p in morePrimes do
        sw.WriteLine p
    sw.Flush()
    sw.Close()

let toNum xs = xs |> List.mapi (fun i t -> (xs.Length - i - 1, t)) |> List.fold (fun acc (i, t) -> acc + (pow 10 (i)) * t) 0

let toNum64 xs = xs |> List.mapi (fun i t -> (xs.Length - i - 1, t)) |> List.fold (fun acc (i, t) -> acc + (pow64 10L (int64(i))) * t) 0L

let combineDigits n1 n2 = 
    let mutable t = n1
    let mutable r = n2
    while r > 0L do
        t <- t * 10L
        r <- r / 10L
    t + n2

let smallestPanDigit = 123456789L

let largestPanDigit =  987654321L

let smallestPan10Digit = 1023456789L

let largestPan10Digit =  9876543210L

let isPanDig10 r = 
    if r < smallestPan10Digit || r > largestPan10Digit then false
    else
        getDigits64 r |> Set.ofList |> Set.count = 10

let isPanDig r = 
    if r < smallestPanDigit || r > largestPanDigit then false
    else
        let s = getDigits64 r |> Set.ofList
        s.Count = 9 && (s.Contains 0L = false)

let isPanDig3 x y z = combineDigits x y |> combineDigits z |> isPanDig

let primeGenFast rangeTo =
    let upperBound = int(Math.Sqrt (float(rangeTo))) + 1
    let primeBits = new BitArray(rangeTo / 2 - 1, true)
    for p in 3..2..upperBound do
        if primeBits.Get ((p - 3) / 2) then
            for pMult in (p * 3)..(2 * p)..rangeTo do
                let i = (pMult - 3) / 2
                primeBits.Set ((pMult - 3) / 2, false)
    [| for i in -1..primeBits.Length - 1 do if i = -1 then yield 2 else if primeBits.Get i then yield (i * 2 + 3) |]

let rec permutationsOf set = 
    let getSplits xs = 
        let rec loop before after =
            match after with
            | hd :: tl -> (before, after) :: (loop (before @ [after.Head]) after.Tail)
            | []       -> [(before, [])]
        loop [] xs
    let extend el xs = getSplits xs |> List.map (fun (before, after) -> before @ [el] @ after)
    match set with
    | el :: nk :: tl ->
        let rest = permutationsOf (nk :: tl)
        rest |> List.collect (fun s -> extend el s)
    | hd :: []       -> [ [ hd ] ]
    | []             -> []

let rec choose set k = 
    if k = 1 then [ for n in set -> [n] ]
    else
        let split xs = 
            let rec loop xs acc =
                match xs with
                | hd :: nk :: tl  -> loop (nk :: tl) ((nk :: tl, hd) :: acc)
                | _       -> acc
            loop xs []
        split set |> List.collect (fun (xs, x) -> choose xs (k - 1) |> List.map (fun t -> x :: t))

let loadStrings fileName = 
    getLines fileName
    |> Array.collect (fun l -> l.Split ',') 
    |> Array.sort
    |> Array.map (fun n -> n.Trim '"' |> Seq.toList)

let getStringScore s =
    let rec loop (s: char list) acc = 
        match s with
        | hd :: tl -> loop tl (acc + System.Convert.ToInt32 hd - System.Convert.ToInt32 'A' + 1)
        | _        -> acc
    loop s 0

let isPrime (primes: int[]) (p: int) =
    if p = 1 then false
    else
        let upperBound = int(Math.Sqrt (float(p))) + 1
        let mutable i = 0
        let mutable r = true
        while r && primes.[i] <= upperBound do
            if p % primes.[i] = 0 then r <- false
            else i <- i + 1
        r

let isPrime64 (primes: int64[]) (p: int64) =
    if p = 1L then false
    else
        let upperBound = int64(Math.Sqrt (float(p))) + 1L
        let mutable i = 0
        let mutable r = true
        while r && primes.[i] <= upperBound do
            if p % primes.[i] = 0L then r <- false
            else i <- i + 1
        r