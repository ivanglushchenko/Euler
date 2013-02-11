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