module Helpers

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

let rec gcd a b = if b = 0L then abs a else gcd b (a % b)

let getFactors target =
    let targetsr = int64(sqrt (float target))
    getPrimes targetsr (fun n -> target % (int64(n)) = 0L)

let toInt c = System.Int32.Parse(c.ToString())

let getLines s = System.IO.File.ReadAllLines s

let loadMatrix fn = getLines fn |> Array.map (fun s -> s.Split ' ' |> Array.map (fun t -> System.Int64.Parse t))

let pow n p = int64(System.Math.Pow(float(n), float(p)))

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
        |> List.map (fun (p, a) -> (pow p (a + 1L) - 1L) / (p - 1L))
        |> List.fold (fun acc t -> acc * t) 1L
    t - n