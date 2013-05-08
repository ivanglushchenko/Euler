open Helpers
open System
open System.Collections
open System.Numerics

let problem1 () =
    seq { for n in 1..999 do if n % 3 = 0 || n % 5 = 0 then yield n } |> Seq.sum

let problem2 () =
    let rec fib x y = 
        seq { yield x
              yield! fib y (x + y) }
    fib 1 2 |> Seq.takeWhile (fun f -> f < 4000000) |> Seq.filter (fun f -> f % 2 = 0) |> Seq.sum

let problem3 () =
    600851475143L |> getFactors |> List.max

let problem4 () = 
    let isPoly x = 
        let s = x.ToString()
        if s.[0] <> s.[s.Length - 1] then false
        else if s.[1] <> s.[s.Length - 2] then false
        else if s.[2] <> s.[s.Length - 3] then false
        else true

    let all3Digits = [| 100..999 |]
    let allProducts = [| for x in all3Digits do
                            for y in all3Digits do
                                let prod = x * y
                                if isPoly prod then yield prod |]
    allProducts |> Array.max

let problem5 () = 
    let nums = [ 1L..20L ]
    let primes = getAllPrimes 20L
    let prod = primes |> List.fold (fun acc p -> acc * p) 1L

    let rec increase l p =
        match l with
        | hd :: tl -> 
            if p % hd <> 0L then
                let t = gcd p hd
                let r = hd / t
                increase tl (p * r)
            else increase tl p
        | [] -> p

    increase nums prod

let problem6 () = 
    let n = 100
    let sumOfSquares = [| for x in 1..n -> x * x |] |> Array.sum
    let squareOfSum = 
        let t = [| for x in 1..n -> x |] |> Array.sum
        t * t
    squareOfSum - sumOfSquares

// 104743
let problem7 () =
    let getNthPrime target nth =
        let all = [| 3..2..target |] 
        let rec sieve all k =
            if Array.isEmpty all then -k
            else if k = nth then all.[0]
            else 
                let nextPrime = all.[0]
                sieve (all |> Array.filter (fun n -> n % nextPrime <> 0)) (k + 1)
        sieve all 2
    getNthPrime 10000000 10001

// 40824
let problem8 () =
    let s = "7316717653133062491922511967442657474235534919493496983520312774506326239578318016984801869478851843858615607891129494954595017379583319528532088055111254069874715852386305071569329096329522744304355766896648950445244523161731856403098711121722383113622298934233803081353362766142828064444866452387493035890729629049156044077239071381051585930796086670172427121883998797908792274921901699720888093776657273330010533678812202354218097512545405947522435258490771167055601360483958644670632441572215539753697817977846174064955149290862569321978468622482839722413756570560574902614079729686524145351004748216637048440319989000889524345065854122758866688116427171479924442928230863465674813919123162824586178664583591245665294765456828489128831426076900422421902267105562632111110937054421750694165896040807198403850962455444362981230987879927244284909188845801561660979191338754992005240636899125607176060588611646710940507754100225698315520005593572972571636269561882670428252483600823257530420752963450"
    let ints = s |> Seq.map (fun c -> System.Convert.ToInt32 (c.ToString())) |> Seq.toList
    let rec getMax l m =
        match l with
        | i1 :: i2 :: i3 :: i4 :: i5 :: tl -> 
            let prod = i1 * i2 * i3 * i4 * i5
            if m < prod then getMax (i2 :: i3 :: i4 :: i5 :: tl) prod
            else getMax (i2 :: i3 :: i4 :: i5 :: tl) m
        | _ -> m
    getMax ints 0

// 31875000
let problem9 () =
    let a = 
        seq { for x in 1..1000 do
                for y in x..1000 do
                    for z in y..1000 do
                        if x + y + z = 1000 && (x*x + y*y = z*z) then yield x*y*z }
    let t = a |> Seq.take 1 |> Seq.toArray
    0

let problem10 () = 
    let primes = getAllPrimes 2000000L
    primes |> List.fold (fun acc t -> acc + int64(t)) 0L

// 70600674
let problem11 () =
    let m = loadMatrix "P11Input.txt"
    let getHorProd i j =
        m.[i].[j] * m.[i].[j + 1] * m.[i].[j + 2] * m.[i].[j + 3]
    let getVertProd i j =
        m.[i].[j] * m.[i + 1].[j] * m.[i + 2].[j] * m.[i + 3].[j]
    let getDiagProd i j =
        m.[i].[j] * m.[i + 1].[j + 1] * m.[i + 2].[j + 2] * m.[i + 3].[j + 3]
    let getDiagProd2 i j =
        m.[i].[j + 3] * m.[i + 1].[j + 2] * m.[i + 2].[j + 1] * m.[i + 3].[j]
    let maxHorProduct = 
        seq { for i in 0..19 do
                for j in 0..16 do
                    yield getHorProd i j } |> Seq.max
    let maxVertProduct = 
        seq { for i in 0..16 do
                for j in 0..19 do
                    yield getVertProd i j } |> Seq.max
    let maxDiagProduct = 
        seq { for i in 0..16 do
                for j in 0..16 do
                    yield getDiagProd i j } |> Seq.max
    let maxDiagProduct2 = 
        seq { for i in 0..16 do
                for j in 0..16 do
                    yield getDiagProd2 i j } |> Seq.max
    max (max (max maxHorProduct maxVertProduct) maxDiagProduct) maxDiagProduct2

// explanation for the math: http://code.jasonbhill.com/sage/project-euler-problem-12/
let problem12 () = 
    let primes = loadPrimes ()
    let getFactorsSum n =
        if n = 1L then 2L
        else
            let mutable remainder = n
            let mutable factorsProduct = 1L
            let mutable primeIndex = 0
            while remainder > 1L && primeIndex < primes.Length do
                let prime = primes.[primeIndex]
                let mutable primeDegree = 0L
                while remainder % prime = 0L do
                    primeDegree <- primeDegree + 1L
                    remainder <- remainder / prime
                primeIndex <- primeIndex + 1
                if primeDegree > 0L then
                    factorsProduct <- factorsProduct * (if prime = 2L then primeDegree else primeDegree + 1L)
            factorsProduct

    let rec checkNext (n, factorsProduct) = 
        let nextn = n + 1L
        let nextFactorsProduct = getFactorsSum nextn
        let triangleNumber = n * nextn / 2L
        let triangleFactors = factorsProduct * nextFactorsProduct
        if triangleFactors > 500L then triangleNumber
        else checkNext (nextn, nextFactorsProduct);

    checkNext (1L, getFactorsSum 1L)

let problem13 () =
    getLines "P13Input.txt" |> Array.map bigint.Parse |> Array.sum

let problem14 () =
    let getSeqLength startingPoint =
        let mutable x = startingPoint
        let mutable length = 1
        while x <> 1L do
            length <- length + 1
            x <- if x % 2L = 0L then x / 2L else x * 3L + 1L
        (length, startingPoint)
    let test = getSeqLength 13L
    seq { for n in 1L..999999L -> getSeqLength n } |> Seq.max |> snd

let problem15 () =
    let n = 21
    let grid = [| for i in 1..n -> [| for j in 1..n -> 0L |] |]
    grid.[0].[0] <- 1L
    for layer in 2..n * 2 - 1 do
        let layerSize = if layer > n then 2 * n - layer else layer
        for l in 1..layerSize do
            let i = if layer > n then n - l else layer - l
            let j = if layer > n then layer - n + l - 1 else l - 1
            if i > 0 then grid.[i].[j] <- grid.[i].[j] + grid.[i - 1].[j]
            if j > 0 then grid.[i].[j] <- grid.[i].[j] + grid.[i].[j - 1]
    grid.[n - 1].[n - 1]

let problem16 () = 
    BigInteger.Pow(bigint(2), 1000).ToString() |> Seq.map toInt |> Seq.sum

let problem17 () =
    let digitToString n = 
        match n with
        | 1 -> "one"
        | 2 -> "two"
        | 3 -> "three"
        | 4 -> "four"
        | 5 -> "five"
        | 6 -> "six"
        | 7 -> "seven"
        | 8 -> "eight"
        | 9 -> "nine"
        | _ -> raise (new System.Exception())
    let tensToString d n  = 
        match (d, n) with
        | (1, 0) -> "ten"
        | (1, 1) -> "eleven"
        | (1, 2) -> "twelve"
        | (1, 3) -> "thirteen"
        | (1, 4) -> "fourteen"
        | (1, 5) -> "fifteen"
        | (1, 8) -> "eighteen"
        | (1, _) -> digitToString n + "teen"
        | (2, _) -> "twenty" + if n > 0 then "-" + digitToString n else ""
        | (3, _) -> "thirty" + if n > 0 then "-" + digitToString n else ""
        | (4, _) -> "forty" + if n > 0 then "-" + digitToString n else ""
        | (5, _) -> "fifty" + if n > 0 then "-" + digitToString n else ""
        | (6, _) -> "sixty" + if n > 0 then "-" + digitToString n else ""
        | (7, _) -> "seventy" + if n > 0 then "-" + digitToString n else ""
        | (8, _) -> "eighty" + if n > 0 then "-" + digitToString n else ""
        | (9, _) -> "ninety" + if n > 0 then "-" + digitToString n else ""
        | _ -> raise (new System.Exception())

    let numberToString n =
        if n = 1000 then "one thousand"
        else
            let xs = n.ToString() |> Seq.toList
            let toInt c = System.Int32.Parse(c.ToString())
            let (hundreds, tens, ones) = 
                match xs with
                    | c1 :: c2 :: c3 :: [] -> (toInt c1, toInt c2, toInt c3)
                    | c1 :: c2 :: []       -> (0, toInt c1, toInt c2)
                    | c1 :: []             -> (0, 0, toInt c1)
                    | _                    -> raise (new System.Exception())
            let s = 
                if hundreds > 0 then 
                    if tens > 0 || ones > 0 then (digitToString hundreds) + " hundred and " 
                    else (digitToString hundreds) + " hundred "
                else ""
            let s = 
                if tens > 0 then s + tensToString tens ones 
                else if ones > 0 then s + digitToString ones else s
            s
    let getCount s = s |> Seq.filter (fun c -> System.Char.IsLetter c) |> Seq.length
    let numbers = [| for n in 1..1000 -> numberToString n |]
    numbers |> Array.map getCount |> Array.sum

let problem18 () = 
    let lines = loadMatrix "P18Input.txt"
    let n = lines.Length
    let grid = [| for i in 1..n -> [| for j in 1..n -> 0L |] |]
    grid.[0].[0] <- lines.[0].[0]
    for i in 2..n do
        for j in 1..i do
            let m1 = if i > j then lines.[i - 1].[j - 1] + grid.[i - 2].[j - 1] else 0L
            let m2 = if j > 1 then lines.[i - 1].[j - 1] + grid.[i - 2].[j - 2] else 0L
            grid.[i - 1].[j - 1] <- max m1 m2
    grid.[n - 1] |> Array.max

let problem19 () = 
    let startDate = new System.DateTime(1901, 1, 1)
    let rec loop (dt: System.DateTime) acc =
        if dt.Year > 2000 then acc
        else if dt.DayOfWeek = System.DayOfWeek.Sunday then loop (dt.AddMonths 1) (acc + 1)
        else loop (dt.AddMonths 1) acc
    loop startDate 0

let problem20 () =
    let rec facRec (n: bigint) c =
        if c <= 1 then n
        else facRec (BigInteger.Multiply(n, bigint(c))) (c - 1)
    let fac (n: int) = facRec (bigint(n)) (n - 1)
    (fac 100).ToString() |> Seq.map toInt |> Seq.sum

let problem21 () = 
    let limit = 10000L
    let primes = getAllPrimes limit |> List.rev |> List.toArray
    let map = [ for x in 1L..limit - 1L -> (x, getProperDivisorsSum x primes) ] |> Map.ofList
    let amicableNumbers = map |> Map.toSeq |> Seq.filter (fun (x, y) -> x <> y && map.ContainsKey y && map.[y] = x)
    (amicableNumbers |> Seq.map (fun (x, y) -> x + y) |> Seq.sum) / 2L

let problem22 () = 
    let names = loadStrings "P22Input.txt"
    names |> Array.mapi (fun i n -> (i + 1) * (getStringScore n)) |> Array.sum

// 4179871
let problem23 () =
    let upperLimit = 29123L
    let primes = getAllPrimes upperLimit |> List.rev |> List.toArray
    let divs = [ for x in 1L..upperLimit -> (x, getProperDivisorsSum x primes) ]
    let divsMap = divs |> Map.ofList
    let abudantNumbers = divs |> List.filter (fun (k, v) -> v > k) |> List.map fst |> Set.ofList
    let isNotSumOfAbudantNumbers n = 
        abudantNumbers |> Set.forall (fun a -> abudantNumbers.Contains (n - a) = false)
    let nonFactorizableNumbers =
        seq { for i in 1L..upperLimit do
                if isNotSumOfAbudantNumbers i then yield i }
    nonFactorizableNumbers |> Seq.sum

// 2783915460
let problem24 () =
    let s = [ for x in 0..9 -> x.ToString() ]
    let allPerutations = permutationsOf s |> List.map (fun s -> s |> List.fold (fun acc i -> acc + i) "") |> List.sort
    List.nth allPerutations 999999

// 4782
let problem25 () = 
    let mutable f1 = bigint(1)
    let mutable f2 = bigint(1)
    let mutable i = 2
    while f2.ToString().Length < 1000 do
        let sum = f1 + f2
        f1 <- f2
        f2 <- sum
        i <- i + 1
    i

// 983
let problem26 () =
    let getCycleLength n =
        let rec loop d s = 
            let r = d % n
            if r = 0 || s |> Set.contains r then s.Count
            else loop (r * 10) (s.Add r)
        loop 1 Set.empty
    seq { for n in 1..999 -> (getCycleLength n, n) } |> Seq.maxBy fst |> snd

// -59231
let problem27 () =
    let f n a b = n * n + a * n + b
    let primes = getAllPrimes 10000L 
    let primesSet = primes |> Set.ofList
    let choicesForB = [ for x in primes |> List.filter (fun p -> p < 1000L) -> [ x; -x ] ] |> List.concat
    let coefs =
        seq { for a in -999L..999L do
                for b in choicesForB do yield (a, b) }
    let getMaxNumOfPrimes (a, b) =
        let rec loop i =
            let p = f i a b
            if primesSet.Contains p then loop (i + 1L)
            else i
        loop 0L
    let (a, b) = coefs |> Seq.map (fun t -> (getMaxNumOfPrimes t, t)) |> Seq.maxBy fst |> snd
    a * b

// 669171001
let problem28 () =
    let upperRight  n = (2 * n + 1) * (2 * n + 1)
    let upperLeft   n = (2 * n + 1) * (2 * n + 1) - 2 * n
    let lowerLeft   n = (2 * n + 1) * (2 * n + 1) - 4 * n
    let lowerRight  n = (2 * n + 1) * (2 * n + 1) - 6 * n
    let sumOfDiag n = if n = 0 then 1 else upperRight n + upperLeft n + lowerLeft n + lowerRight n
    seq { for n in 0..500 -> sumOfDiag n } |> Seq.sum

// 9183
let problem29 () =
    let distinctNums =
        seq { for a in 2..100 do
                for b in 2..100 do
                    yield BigInteger.Pow(bigint(a), b).ToString() } |> Set.ofSeq
    distinctNums.Count

// 443839
let problem30 () =
    let getSumOfDigPow n = getDigits n |> List.map (fun n -> pow n 5) |> List.sum
    seq { for n in 10..9999999 do
            let sum = getSumOfDigPow n
            if sum = n then yield n } |> Seq.sum

// 73682
let problem31 () =
    let rec getCombinations coins sum =
        if sum = 0 then [ [ ] ]
        else 
            match coins with
            | coin :: nk :: tl ->
                let allCoinCombinations = [ for i in 0..sum / coin -> ([ for j in 1..i -> coin ], sum - coin * i) ]
                let extendCoinCombination (partialSolution, rest) = 
                    let combinationForRest = getCombinations (nk :: tl) rest
                    [ for sln in combinationForRest -> partialSolution @ sln ]
                allCoinCombinations |> List.collect extendCoinCombination
            | coin :: [] ->
                let (d, r) = (sum / coin, sum % coin)
                if r = 0 then [ [ for i in 1..d -> coin ] ]
                else []
            | [] -> []
    getCombinations [ 1; 2; 5; 10; 20; 50; 100; 200 ] 200 |> List.length

// 45228
let problem32 () = 
    let isPanDig2 x y = isPanDig3 x y (x * y)
    let t = isPanDig2 19L 657L
    let t1 = isPanDig2 7L 1470L
    let allProds = 
        seq { for i in 1L..99L do
                for j in 123L..9876L do
                    if isPanDig2 i j then yield (i * j, (i, j)) } |> Map.ofSeq
    allProds |> Map.toSeq |> Seq.map fst |> Seq.sum

// 100
let problem33 () =
    let isCurious (n1, n2) (d1, d2) =
        let num = n1 * 10 + n2
        let den = d1 * 10 + d2
        let check n d = num * d = n * den
        if num = 0 || den = 0 || num >= den || (n2 = 0 && d2 = 0) then false
        else
            if n1 = d1 then check n2 d2
            else if n2 = d2 then check n1 d1
            else if n2 = d1 then check n1 d2
            else if n1 = d2 then check n2 d1
            else false
    let allNums = 
        seq { for n1 in 1..9 do
                for n2 in 0..9 do
                    for d1 in 1..9 do
                        for d2 in 0..9 do
                            if isCurious (n1, n2) (d1, d2) then yield (n1 * 10 + n2, d1 * 10 + d2) }// |> Seq.toArray
    let prod = allNums |> Seq.fold (fun (an, ad) (n, d) -> (an * n, ad * d)) (1, 1)
    let g = gcd (fst prod) (snd prod)
    snd prod / g

// 40730
let problem34 () = 
    let rec factorial n = if n <= 1 then 1 else n * factorial (n - 1)
    let isCurious n = 
        if n <= 2 then false
        else
            let digits = getDigits n
            n = (digits |> List.map (fun t -> factorial t) |> List.sum)
    seq { for n in 3..999999 do
            if isCurious n then yield n } |> Seq.sum
    
// 55
let problem35 () =
    let primes = loadPrimes () |> Array.filter (fun p -> p < 1000000L)
    let primesMap = primes |> Set.ofArray
    let isCircular p = 
        let digits = getDigits64 p
        if digits.Length = 1 then true
        else
            let rec loop xs mult acc = 
                match xs with
                | hd :: nk :: tl ->
                    let rest = (acc * 10L) + hd
                    let t = (toNum64 (nk :: tl)) * mult + rest
                    if primesMap.Contains t then
                        loop (nk :: tl) (mult * 10L) rest
                    else false
                | _ -> true
            loop digits 10L 0L
    primes |> Array.filter isCircular |> Array.length

// 872187
let problem36 () =
    let isPalBase10 n =
        let a = getDigits n |> Array.ofList
        seq { for i in 0..a.Length / 2 -> i } |> Seq.forall (fun i -> a.[i] = a.[a.Length - i - 1])
    let isPalBase2 (n: int) =
        let b = new BitArray([| n |])
        let bound = seq { for i in 31..-1..0 do
                            if b.[i] then yield i } |> Seq.take 1 |> Seq.head
        seq { for i in 0..bound / 2 -> i } |> Seq.forall (fun i -> b.[i] = b.[bound - i])
    let nums =
        seq { for i in 1..999999 do
                if isPalBase10 i && isPalBase2 i then yield i }
    nums |> Seq.sum
  
// 748317  
let problem37 () =
    let primes = loadPrimes () |> Array.filter (fun p -> p < 1000000L)
    let primesMap = primes |> Set.ofArray
    let isPrimeTruncatable p =
        if p < 10L then false
        else
            let rec checkTails xs =
                match xs with
                | hd :: nk :: tl -> if primesMap.Contains (toNum64 (nk :: tl)) then checkTails (nk ::tl) else false
                | _              -> true
            let rec checkHeads n =
                if n > 1L then
                    let t = n / 10L
                    if t = 0L || primesMap.Contains t then checkHeads t else false
                else true
            if checkHeads p then
                getDigits64 p |> checkTails
            else false
    primes |> Array.filter isPrimeTruncatable |> Array.sum

// 932718654
let problem38 () =
    seq { for i in 9123L..9876L do
            let d = combineDigits i (i * 2L)
            if isPanDig d then yield d } |> Seq.max

// 840
let problem39 () =
    seq { for p in 12..1000 do
            for b in 1..p - 1 do
                let num = 2 * p * b - p * p
                let den = 2 * (b - p)
                let (a, r) = (num / den, num % den)
                if r = 0 && a > 0 && a >= b then yield (p, (b, a, p - b - a)) } |> Seq.groupBy fst |> Seq.maxBy (fun t -> snd t |> Seq.length) |> fst

// 210
let problem40 () =
    let rec digits n = 
        seq { for i in getDigits n do
                yield i
              yield! digits (n + 1)}
    let d = digits 1 |> Seq.take 1000000 |> Seq.toArray
    d.[0] * d.[9] * d.[99] * d.[999] * d.[9999] * d.[99999] * d.[999999]

// 7652413
let problem41 () =
    let primes = primeGenFast 10000000 |> Set.ofArray
    seq { for i in 1234567..7654321 do
            if primes.Contains i && isPanDig3 (int64(i)) 8L 9L then yield i } |> Seq.max

// 162
let problem42 () =
    let scores = 
        loadStrings "P42Input.txt"
        |> Array.map (fun n -> getStringScore n)
    let maxScore = scores |> Array.max
    let triangleNums = Seq.initInfinite (fun i -> (i + 1) * (i + 2) / 2) |> Seq.takeWhile (fun n -> n <= maxScore) |> Set.ofSeq
    scores |> Array.filter (fun n -> triangleNums.Contains n) |> Array.length

// 16695334890
let problem43 () =
    let primes = primeGenFast 100 |> Array.mapi (fun i p -> (i, int64(p))) |> Map.ofArray
    let rec isInterestingNumber n =
        let rec loop n i = 
            match n with
            | p1 :: p2 :: p3 :: tl ->
                let num = toNum64 [ p1; p2; p3 ]
                let den = primes.[i]
                if num % den = 0L then loop (p2 :: p3 :: tl) (i + 1) else false
            | _       -> true
        loop (n |> List.tail) 0
    let digits = [ for i in 0L..9L -> i ]
    let allPerutations = 
        permutationsOf digits
        |> List.filter (fun l -> l |> List.head <> 0L && isInterestingNumber l)
        |> List.map (fun l -> toNum64 l)
    allPerutations |> List.sum

// 5482660
let problem44 () = 
    let pentagonalNumsSeq = Seq.initInfinite (fun i -> (i + 1, (i + 1) * ((i + 1) * 3 - 1) / 2)) |> Seq.take 10000
    let pnMap = pentagonalNumsSeq |> Map.ofSeq
    let pnReverseMap = pentagonalNumsSeq |> Seq.map (fun (k, v) -> (v, k)) |> Map.ofSeq
    let rec checkIth i currentMin =
        if i > pnMap.Count || pnMap.[i] - pnMap.[i - 1] > currentMin then currentMin
        else
            let diffs = seq { for n in 1..i - 1 do
                                let pIth = pnMap.[n]
                                let pJth = pnMap.[i]
                                if pnReverseMap.ContainsKey (pJth - pIth) && pnReverseMap.ContainsKey (pJth + pIth) then yield pJth - pIth }
            if Seq.length diffs > 0 then checkIth (i + 1) (min currentMin (Seq.min diffs))
            else checkIth (i + 1) currentMin
    checkIth 2 (System.Int32.MaxValue)

// 1533776805
let problem45 () =
    let getTriangleNum n = n * (n + 1L) / 2L
    let getPentagonalNum n = n * (3L * n - 1L) / 2L
    let getHexagonalNum n = n * (2L * n - 1L)
    let calcHexagonalNum n2 = 
        let det = 4L - 16L * (n2 - 3L * n2 * n2)
        let sqr = int64(Math.Sqrt (float(det)))
        if sqr * sqr = det then
            let num = 2L + sqr
            let den = 8L
            if num % den = 0L then num / den else 0L
        else 0L
    Seq.initInfinite (fun i -> int64(i) + 166L)
        |> Seq.map (fun n2 -> calcHexagonalNum n2)
        |> Seq.filter (fun n3 -> n3 > 0L)
        |> Seq.map (fun n1 -> getHexagonalNum n1)
        |> Seq.take 1
        |> Seq.head

// 5777
let problem46 () = 
    let twiceSquares = seq { for i in 1..1000 -> 2 * i * i } |> Set.ofSeq
    let primes = primeGenFast 1000000 |> Set.ofArray
    let isComposable n = twiceSquares |> Set.exists (fun sq -> primes.Contains (n - sq))
    let compositeNums = Seq.initInfinite (fun i -> i * 2 + 3) |> Seq.filter (fun n -> primes.Contains n = false && isComposable n = false)
    compositeNums |> Seq.head

// 134043
let problem47 () =
    let primes = primeGenFast 1000000 |> Array.map (fun p -> int64(p))
    let nums = Seq.initInfinite (fun i -> getPrimeDivisors (int64(i) + 2L) primes |> List.length) |> Seq.take 1000000 |> Seq.toList
    let rec getDistinct i nums = 
        match nums with
        | n1 :: n2 :: n3 :: n4 :: tl ->
            if n1 < 4 || n2 < 4 || n3 < 4 || n4 < 4 then getDistinct (i + 1L) (n2 :: n3 :: n4 :: tl)
            else i
        | _ -> 0L
    getDistinct 2L nums

// 9110846700
let problem48 () =
    let n = seq { for n in 1..1000 -> bigint.Pow (bigint(n), n) } |> Seq.sum
    let s = n.ToString()
    s.Substring (s.Length - 10)

// 296962999629
let problem49 () =
    let primes = primeGenFast 9999 |> Array.filter (fun p -> p > 1000)
    let primesSet = primes |> Set.ofArray
    let getEquallyIncreasingSubList ps =
        if ps |> List.length < 3 then []
        else
            let primes = ps.Tail |> Set.ofList
            let rec loop rest =
                match rest with
                | hd :: tl ->
                    let step = hd - ps.Head
                    if primes.Contains (hd + step) then [ps.Head; hd; hd + step ]
                    else loop tl
                | _ -> []
            loop ps.Tail
    let increasingPrimes = 
        seq { for p in primes do
                let permutations = 
                    getDigits p 
                        |> permutationsOf 
                        |> List.map (fun n -> toNum n) 
                        |> List.filter (fun n -> n > p && primesSet.Contains n) 
                        |> (Seq.distinct >> Seq.sort >> Seq.toList)
                let subList = p :: permutations |> getEquallyIncreasingSubList
                if List.isEmpty subList = false then yield subList }
    increasingPrimes |> Seq.filter (fun l -> l.Head <> 1487) |> Seq.head |> List.fold (fun acc t -> acc + (t.ToString())) ""

// 997651
let problem50 () = 
    let primes = primeGenFast 999999
    let primesSums = 
        primes 
        |> Array.fold (fun acc t -> ((if acc.IsEmpty then 0L else acc.Head) + int64(t)) :: acc) [] 
        |> List.filter (fun s -> s < 999999L)
        |> List.rev 
        |> List.toArray
    let primesSet = primes |> Array.map (fun p -> int64(p)) |> Set.ofArray
    seq { for i in 0..primesSums.Length - 2 do
                    for j in i + 1..primesSums.Length - 1 do
                        let sum = primesSums.[j] - primesSums.[i]
                        if primesSet.Contains sum then yield sum } |> Seq.max

// 121313
let problem51 () =
    let primes = primeGenFast 999999 |> Array.filter (fun p -> p >= 100000)
    let primesSet = primes |> Set.ofSeq
    let rec getMasks d n = 
        let updMask xs i =
            if List.nth xs (i - 1) = true then []
            else [ for n in 1..d -> if n = i then true else xs.[n - 1] ]
        if n = 0 then [ [ for i in 1..d -> false ] ]
        else getMasks d (n - 1) |> List.collect (fun mask -> [ for i in 1..d -> updMask mask i ]) |> Seq.filter (fun mask -> mask.IsEmpty = false) |> Seq.distinct |> Seq.toList
    let applyMask bs n r = 
        let rec loop bs n exp acc = 
            match bs with
            | true :: tl  -> loop tl (n / 10) (exp * 10) (acc - exp * (n % 10) + exp * r)
            | false :: tl -> loop tl (n / 10) (exp * 10) acc
            | []          -> acc
        n + loop bs n 1 0
    let r1 = applyMask [true; false; false ] 123 7
    let r2 = applyMask [false; true; false ] 123 7
    let r3 = applyMask [false; false; true ] 123 7
    let r4 = applyMask [true; false; true ] 123 7
    let getFamily mask p =
        let family = [ for i in 0..9 -> applyMask mask p i ] |> List.filter (fun n -> primesSet.Contains n)
        if family.Length = 8 then Some(family.Head) else None
    seq { for m in 1..5 do
            for mask in getMasks 7 m do
                for p in primes do
                    let family = getFamily mask p
                    match family with
                    | Some(p) -> yield p
                    | None -> () } |> Seq.min

// 142857
let problem52 () =
    let isTheNumber n = 
        let digits = n |> getDigits |> List.sort
        let rec loop m = 
            if m = 7 then true
            else
                let t = n * m |> getDigits |> List.sort
                if t = digits then loop (m + 1) else false
        loop 2
    Seq.initInfinite (fun i -> i + 1) |> Seq.filter isTheNumber |> Seq.head

// 4075
let problem53 () =
    let factorials = 
        seq { 1..100 } 
        |> Seq.fold (fun acc n -> (n, (snd acc.Head) * bigint(n)) :: acc) [ (0, bigint(1)) ]
        |> Map.ofSeq
    let c n r = factorials.[n] / (factorials.[r] * factorials.[n - r])
    let lowerBound = bigint(1000000)
    seq { for n in 2..100 do
            for r in 1..n - 1 do
                let coef = c n r
                if coef > lowerBound then yield coef } |> Seq.length

// 376
let problem54 () =
    let group f hand = hand |> List.map f |> Seq.groupBy (fun i -> i) |> Seq.map (fun (k, v) -> (v |> Seq.length, k)) |> Seq.sortBy (fun (k, v) -> -k) |> List.ofSeq
    let count f hand = group f hand |> List.map fst
    let pick indices l = [ for i in indices -> List.nth l i |> snd ] |> List.sortBy (fun i -> -i)
    let score hand indices = (group fst hand |> pick indices) @ (hand |> List.map fst)
    let dist hand = hand |> List.tail |> List.map fst |> List.fold (fun acc t -> (t, if acc.IsEmpty then fst hand.Head - t else fst acc.Head - t) :: acc) [] |> List.map snd
    let (|OnePair|_|) hand       = if count fst hand = [ 2; 1; 1; 1 ] then Some(score hand [ 0 ]) else None
    let (|TwoPairs|_|) hand      = if count fst hand = [ 2; 2; 1 ] then Some(score hand [ 0; 1 ]) else None
    let (|Three|_|) hand         = if count fst hand |> List.head = 3 then Some(score hand [ 0 ]) else None
    let (|Straight|_|) hand      = if dist hand = [ 1; 1; 1; 1 ] then Some(fst hand.Head) else None
    let (|Flush|_|) hand         = if count snd hand = [ 5 ] then Some(hand) else None
    let (|FullHouse|_|) hand     = if count fst hand = [ 3; 2 ] then Some(score hand [ 0 ]) else None
    let (|Four|_|) hand          = if count fst hand = [ 4; 1 ] then Some(score hand [ 0 ]) else None
    let (|StraightFlush|_|) hand = if dist hand = [ 1; 1; 1; 1 ] && count snd hand = [ 5 ] then Some(fst hand.Head) else None
    let (|RoyalFlush|_|) hand    = if dist hand = [ 1; 1; 1; 1 ] && count snd hand = [ 5 ] && fst hand.Head = 14 then Some(fst hand.Head) else None
    let getSuit input = 
        match input with
        | 'H' -> 1
        | 'S' -> 2
        | 'C' -> 3
        | _   -> 4
    let getValue input =
        match input with
        | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> System.Int32.Parse (input.ToString())
        | 'T' -> 10
        | 'J' -> 11
        | 'Q' -> 12
        | 'K' -> 13
        | _   -> 14
    let getCard (c: string) = (getValue c.[0], getSuit c.[1])
    let getHand cs = cs |> Array.map getCard |> Array.sortBy (fun (v, s) -> -v) |> List.ofArray
    let isWinningHand (hand, adv) = 
        match (hand, adv) with
        | (RoyalFlush(_), _)  -> true
        | (_, RoyalFlush(_))  -> false

        | (StraightFlush(v1), StraightFlush(v2)) -> v1 > v2
        | (StraightFlush(_), _) -> true
        | (_, StraightFlush(_)) -> false

        | (Four(v1), Four(v2)) -> v1 > v2
        | (Four(_), _) -> true
        | (_, Four(_)) -> false

        | (FullHouse(v1), FullHouse(v2)) -> v1 > v2
        | (FullHouse(_), _) -> true
        | (_, FullHouse(_)) -> false

        | (Flush(v1), Flush(v2)) -> v1 > v2
        | (Flush(_), _) -> true
        | (_, Flush(_)) -> false

        | (Straight(v1), Straight(v2)) -> v1 > v2
        | (Straight(_), _) -> true
        | (_, Straight(_)) -> false

        | (Three(v1), Three(v2)) -> v1 > v2
        | (Three(_), _) -> true
        | (_, Three(_)) -> false

        | (TwoPairs(v1), TwoPairs(v2)) -> v1 > v2
        | (TwoPairs(_), _) -> true
        | (_, TwoPairs(_)) -> false
        
        | (OnePair(v1), OnePair(v2)) -> v1 > v2
        | (OnePair(_), _) -> true
        | (_, OnePair(_)) -> false

        | _ -> fst hand.Head > fst adv.Head
    getLines "P54Input.txt" 
        |> Array.map (fun s -> s.Split ' ') 
        |> Array.map (fun a -> (getHand a.[0..4], getHand a.[5..9]))
        |> Array.filter isWinningHand 
        |> Array.length

// 249
let problem55 () =
    let isPalindrome n =
        let s = n.ToString()
        let rec loop i =
            if i > s.Length / 2 then true
            else if s.[i] <> s.[s.Length - 1 - i] then false
            else loop (i + 1)
        loop 0
    let reverse n =
        let a = n.ToString().ToCharArray()
        Array.Reverse a
        new String(a) |> bigint.Parse
    let isLychrelNumber (n: int) =
        let rec loop n i =
            if i > 50 then true
            else
                let rev = reverse n
                let sum = n + rev
                if isPalindrome sum then false else loop sum (i + 1)
        loop (bigint(n)) 1 
    seq { 1..9999 } |> Seq.filter (fun n -> isLychrelNumber n) |> Seq.length

// 972
let problem56 () =
    let getDigitsSum n = n.ToString() |> Seq.map (fun c -> Int32.Parse(c.ToString())) |> Seq.sum
    seq { for a in 1..100 do
            for b in 1..100 do
                yield BigInteger.Pow(bigint(a), b) |> getDigitsSum } |> Seq.max

// 153
let problem57 () =
    let take c =
        let rec loop (n, d) i acc =
            if i > c then acc
            else loop (n + d * bigint(2), n + d) (i + 1) ((n + d * bigint(2), n + d) :: acc)
        loop (bigint 1, bigint 1) 1 [] |> List.rev
    take 1000 |> List.filter (fun (n, d) -> n.ToString().Length > d.ToString().Length) |> List.length

// 26241
let problem58 () =
    let primes = primeGenFast 100000
    let upperRight  n = (2 * n + 1) * (2 * n + 1) - 6 * n
    let upperLeft   n = (2 * n + 1) * (2 * n + 1) - 4 * n
    let lowerLeft   n = (2 * n + 1) * (2 * n + 1) - 2 * n
    let rec loop n acc1 acc2 =
        let check f = if f n |> isPrime primes then 1 else 0
        let t = lowerLeft n
        let totalPrimesCount = acc1 + check upperRight + check upperLeft + check lowerLeft
        let totalCount = acc2 + 4
        let ratio = totalPrimesCount * 100 / totalCount
        if ratio < 10 then 2 * n + 1 else loop (n + 1) totalPrimesCount totalCount
    loop 1 0 1

// 107359
let problem59 () =
    let cypher = getLines "P59Input.txt" |> Array.map (fun s -> s.Split ',' |> Array.map (fun s -> Int32.Parse s)) |> Seq.head
    let keyGen (c1: int) (c2: int) (c3: int) = Seq.initInfinite (fun i -> if i % 3 = 0 then c1 else if i % 3 = 1 then c2 else c3)
    let text = seq { for c1 in Convert.ToInt32 'a'..Convert.ToInt32 'z' do
                        for c2 in Convert.ToInt32 'a'..Convert.ToInt32 'z' do
                            for c3 in Convert.ToInt32 'a'..Convert.ToInt32 'z' do
                                let text = [| for i in 0..cypher.Length - 1 -> (if i % 3 = 0 then c1 else if i % 3 = 1 then c2 else c3) ^^^ cypher.[i] |> Convert.ToChar |]
                                let index = text |> Seq.filter (fun c -> Char.IsLetter c || Char.IsWhiteSpace c) |> Seq.length
                                if index > int(float(text.Length) * 0.95) then yield text } |> Seq.head
    text |> Seq.map (fun c -> Convert.ToInt32 c) |> Seq.sum

// 26033
let problem60 () =
    let upperBound = 20000
    let primes = primeGenFast 10000000 |> Array.map (fun p -> int64 p)
    let primesSet = primes |> Set.ofArray
    let maxKnownPrime = primes |> Array.max
    let prime p = if p < maxKnownPrime then primesSet.Contains p else isPrime64 primes p
    let isRemarkablePair p1 p2 = prime (combineDigits (int64 p1) (int64 p2)) && prime (combineDigits (int64 p2) (int64 p1))
    let primesToConsider = primes |> Array.filter (fun p -> p < int64 upperBound)
    seq { for p1 in primesToConsider do
            for p2 in primesToConsider do
                if p2 > p1 && isRemarkablePair p1 p2 then
                    for p3 in primesToConsider do
                        if p3 > p2 && isRemarkablePair p1 p3 && isRemarkablePair p2 p3 then
                            for p4 in primesToConsider do
                                if p4 > p3 && isRemarkablePair p1 p4 && isRemarkablePair p2 p4 && isRemarkablePair p3 p4 then
                                    for p5 in primesToConsider do
                                        if p5 > p4 && isRemarkablePair p1 p5 && isRemarkablePair p2 p5 && isRemarkablePair p3 p5 && isRemarkablePair p4 p5 then
                                            yield p1 + p2 + p3 + p4 + p5 } |> Seq.min

// 28684
let problem61 () =
    let generate f = seq { for n in 1..1000 -> f n } |> Seq.filter (fun n -> n >= 1000 && n <= 9999) |> Seq.map (fun n -> (n / 100, (n % 100, n))) |> Map.ofSeq
    let triangleNums = generate (fun n -> n * (n + 1) / 2)
    let squareNums = generate (fun n -> n * n)
    let pentagonalNums = generate (fun n -> n * (3 * n - 1) / 2)
    let hexagonalNums = generate (fun n -> n * (2 * n - 1))
    let heptagonalNums = generate (fun n -> n * (5 * n - 3) / 2)
    let octagonalNums = generate (fun n -> n * (3 * n - 2))

    let merge g1 g2 = g1 |> Map.filter (fun k v -> Map.containsKey (fst v) g2) |> Map.map (fun k v -> (fst g2.[fst v], snd v + snd g2.[fst v]))

    let mergeMany gs = gs |> List.tail |> List.fold (fun acc g -> merge acc g) gs.Head

    permutationsOf [ triangleNums; squareNums; pentagonalNums; hexagonalNums; heptagonalNums; octagonalNums ]
        |> List.map mergeMany
        |> List.collect (fun t -> t |> Map.toList)
        |> List.filter (fun (f, (t, c)) -> f = t)
        |> List.map (snd >> snd)
        |> List.max

// 127035954683
let problem62 () = 
    let upperBound = 10000
    let digitsOf n = 
        n.ToString() 
        |> Seq.map (fun c -> c.ToString() |> Int32.Parse) 
        |> Seq.groupBy (fun c -> c) 
        |> Seq.map (fun (k, v) -> (k, v |> Seq.length)) 
        |> Seq.sort
        |> List.ofSeq
    let cube = 
        seq { for n in 1..upperBound -> (n, BigInteger.Pow(bigint n, 3)) }
        |> Seq.map (fun (k, v) -> (k, digitsOf v))
        |> Seq.groupBy (fun (k, v) -> v) 
        |> Seq.filter (fun (k, v) -> v |> Seq.length = 5)
        |> Seq.map (fun (k, v) -> v |> Seq.map fst |> Seq.min)
        |> Seq.min
    BigInteger.Pow(bigint  cube, 3)

// 49
let problem63 () =
    seq { for n in 0L..9L do
            for p in 1L..100L do
                let num = pow64 n p
                if num.ToString().Length |> int64 = p then yield 1 } |> Seq.sum

// 1322
let problem64 () =
    let periodOf s =
        let a0 = int (Math.Sqrt (float s))
        if a0 * a0 = s then 0 else
            let rec loop (m, d, a) visited =
                let mNext = d * a - m
                let dNext = (s - mNext * mNext) / d
                let aNext = (a0 + mNext) / dNext
                if visited |> Set.contains (mNext, dNext, aNext) then visited |> Seq.length
                else loop (mNext, dNext, aNext) (visited.Add (mNext, dNext, aNext))
            loop (0, 1, (int (Math.Sqrt (float s)))) Set.empty
    let t = periodOf 7
    seq { for n in 2..10000 -> periodOf n } |> Seq.filter (fun p -> p > 0 && p % 2 = 1) |> Seq.length

// 272
let problem65 () =
    let convergents = [| for n in 1..100 -> bigint 0 |]
    for i in 1..100 do
        convergents.[i - 1] <-
            match i with
            | 1 -> bigint 2
            | 2 -> bigint 3
            | _ -> 
                if i % 3 = 0 then (bigint 2) * (bigint (i / 3)) * convergents.[i - 2] + convergents.[i - 3]
                else convergents.[i - 2] + convergents.[i - 3]
    convergents.[99].ToString() |> Seq.map (fun c -> c.ToString() |> Int32.Parse) |> Seq.sum

// 661
let problem66 () =
    let getFundamentalSolution D =
        let a0 = bigint (Math.Sqrt (float D))
        if a0 * a0 = D then (bigint 0, bigint 0) else
            let rec loop (m, d, a) (n1, n2) (d1, d2) =
                let mNext = d * a - m
                let dNext = (D - mNext * mNext) / d
                let aNext = (a0 + mNext) / dNext
                let numNext = aNext * n1 + n2
                let denNext = aNext * d1 + d2
                if numNext * numNext - D * denNext * denNext = BigInteger.One then (numNext, D)
                else loop (mNext, dNext, aNext) (numNext, n1) (denNext, d1)
            loop (bigint 0, bigint 1, a0) (a0, bigint 1) (bigint 1, bigint 0)
    seq { for D in 1..1000 -> 
            getFundamentalSolution (bigint D) } |> Seq.maxBy (fun (x, d) -> x) |> snd

// 7273
let problem67 () =
    let lines = loadMatrix "P67Input.txt"
    let n = lines.Length
    let grid = [| for i in 1..n -> [| for j in 1..n -> 0L |] |]
    grid.[0].[0] <- lines.[0].[0]
    for i in 2..n do
        for j in 1..i do
            let m1 = if i > j then lines.[i - 1].[j - 1] + grid.[i - 2].[j - 1] else 0L
            let m2 = if j > 1 then lines.[i - 1].[j - 1] + grid.[i - 2].[j - 2] else 0L
            grid.[i - 1].[j - 1] <- max m1 m2
    grid.[n - 1] |> Array.max

// 6531031914842725
let problem68 () =
    let validateSums (vs: int[]) = 
        let s1 = vs.[0] + vs.[5] + vs.[6]
        let s2 = vs.[1] + vs.[6] + vs.[7]
        if s1 = s2 then
            let s3 = vs.[2] + vs.[7] + vs.[8]
            if s2 = s3 then
                let s4 = vs.[3] + vs.[8] + vs.[9]
                if s3 = s4 then
                    let s5 = vs.[4] + vs.[9] + vs.[5]
                    s4 = s5 && s5 = s1
                else false
            else false
        else false
                    
    let sln = permutationsOf [ 10; 9; 8; 7; 5; 4; 3; 2; 1 ] |> List.map (fun l -> 6 :: l |> List.toArray) |> List.filter validateSums |> List.max
    sprintf "%i%i%i%i%i%i%i%i%i%i%i%i%i%i%i" sln.[0]  sln.[5]  sln.[6] sln.[1] sln.[6] sln.[7] sln.[2] sln.[7] sln.[8] sln.[3] sln.[8] sln.[9] sln.[4] sln.[9] sln.[5]

// 510510
let problem69 () =
    let upperBound = 1000000
    let primes = primeGenFast 1000 |> List.ofArray
    let rec increaseProduct num prod primes =
        let nextPrime = primes |> List.head
        let nextNum = num * nextPrime
        if nextNum > upperBound then num
        else increaseProduct nextNum ((1.0 - 1.0 / (nextPrime |> float)) * prod) (primes |> List.tail)
    increaseProduct 1 1.0 primes

// 8319823
let problem70 () =
    let upperBound = 10000000
    let primes = primeGenFast 10000
    let getPhi p1 p2 = (p1 - 1) * (p2 - 1)
    let digitsOf d = d.ToString() |> Seq.groupBy (fun c -> c) |> Seq.map (fun (k, v) -> (k, v |> Seq.length)) |> Seq.sort |> Seq.toList
    seq { for p1 in primes do
            for p2 in primes do
                let num = p1 * p2
                if p1 <> p2 && num <= upperBound then
                    let phi = getPhi p1 p2
                    if digitsOf num = digitsOf phi then yield (num, float num / float phi) } |> Seq.minBy snd |> fst

// 428570
let problem71 () =
    let lessThan n1 d1 n2 d2 = d2 * n1 < n2 * d1
    let lessThanTarget n d = lessThan n d 3 7
    let rec nextApproximation d bestNum bestDen =
        if d > 1000000 then bestNum
        else
            let lowerBound = 3 * d / 7
            let rec iterateThroughNums n =
                if lessThanTarget n d then
                    if lessThan n d bestNum bestDen then iterateThroughNums (n + 1) 
                    else  Some(n)
                else None
            match iterateThroughNums lowerBound with
            | Some(n) -> nextApproximation (d + 1) lowerBound d
            | None    -> nextApproximation (d + 1) bestNum bestDen
    nextApproximation 1 0 1

// 303963552391
let problem72 () =
    let primes = primeGenFast 1000000 |> Array.map (fun p -> int64 p)
    seq { for n in 2L..1000000L -> getPrimeDivisors n primes 
                                    |> List.map fst
                                    |> List.fold (fun acc p -> acc * (1.0 - 1.0 / (float p))) (float n)
                                    |> int64
                                    } |> Seq.sum

// 7295372
// This method heavily relies on tail recursion, so use release conf (or use --tailcalls)
let problem73 () =
    let isLess (a, b) (c, d) = a * d < c * b
    let n = 12000
    let nextFarey (a, b) (c, d) = 
        let k = int((n + b) / d)
        (k * c - a, k * d - b)
    let rec countFareys (a, b) (c, d) count = 
        let next = nextFarey (a, b) (c, d)
        if isLess (1, 2) next then count
        else countFareys (c, d) next (if isLess (1, 3) next then count + 1 else count)
    (countFareys (0, 1) (1, n) 0) - 1

// 402
let problem74 () = 
    let factorials = [| 1L; 1L; 2L; 6L; 24L; 120L; 720L; 5040L; 40320L; 362880L |]
    let factorialsSum n = getDigits64 n |> List.map (fun d -> factorials.[int d]) |> List.sum
    let rec calcChainLength lengths n = 
        if lengths |> Map.containsKey n then lengths
        else
            let sum = factorialsSum n
            if sum = n then Map.add n 0L lengths
            else
                let lengths = calcChainLength (Map.add n (0L) lengths) sum
                Map.add n (lengths.[sum] + 1L) lengths
    seq { 1L..1000000L } 
        |> Seq.fold calcChainLength Map.empty 
        |> Map.toSeq 
        |> Seq.filter (fun (k, v) -> v >= 60L) 
        |> Seq.length

// 161667
let problem75 () =
    let upperBound = 1500000
    let a = Math.Sqrt (float upperBound) |> int
    let getTriplet m n = (m * m - n * n, 2 * m * n, m * m + n * n)
    seq { for m in 2..a do
            for n in 1..m - 1 do
                if (m + n) % 2 = 1 && gcd n m = 1 then yield getTriplet m n } 
                |> Seq.collect (fun (a, b, c) ->
                    let rec getAllTriplets k acc = 
                        if a * k + b * k + c * k > upperBound then acc
                        else getAllTriplets (k + 1) ((a * k, b * k, c * k) :: acc)
                    getAllTriplets 1 []) 
                |> Seq.groupBy (fun (a, b, c) -> a + b + c) 
                |> Seq.filter (fun (k, v) -> v |> Seq.length = 1) 
                |> Seq.length

// 190569291
let problem76 () =
    let sum = 100
    let solutions = [| for i in 0..sum -> if i = 0 then 1 else 0 |]
    let rec processNextCoin coins =
        match coins with
        | coin :: tl ->
            for i in coin..sum do
                solutions.[i] <- solutions.[i] + solutions.[i - coin]
            processNextCoin tl
        | _ -> solutions
    Array.get (processNextCoin [ 1..99 ]) sum

// 71
let problem77 () =
    let primes = primeGenFast 5000 |> List.ofArray
    let getMaxSolution sum = 
        let solutions = [| for i in 0..sum -> if i = 0 then 1 else 0 |]
        let rec processNextCoin coins =
            match coins with
            | coin :: tl ->
                for i in coin..sum do
                    solutions.[i] <- solutions.[i] + solutions.[i - coin]
                processNextCoin tl
            | _ -> solutions    
        processNextCoin primes
    let rec getFirst5kSln n =
        let sln = getMaxSolution n
        if sln |> Array.exists (fun t -> t > 5000) then n
        else getFirst5kSln (n + 1)
    getFirst5kSln 1

// 55374
let problem78 () =
    let findNum f = 
        let rec checkNextNumber k (cache: Map<int, bigint>) =
            let rec loop m acc =
                let n = (3 * m - 1) * m / 2
                let res = if n > k then bigint 0 else cache.[k - n]
                let sign = if m % 2 = 0 then bigint -1 else bigint 1
                if res > bigint 0 then 
                    if m < 0 then loop (-m + 1) (acc + res * sign)
                    else loop -m (acc + res * sign)
                else acc
            let res = loop 1 (bigint 0)
            if f k res then k
            else checkNextNumber (k + 1) (Map.add k res cache)
        Map.ofList [(0, bigint 1)] |> checkNextNumber 1
    findNum (fun i p -> p % (bigint 1000000) = bigint 0)

// 73162890
let problem79 () =
    let constraints = getLines "P79Input.txt" |> Array.map (fun t -> t |> Seq.map toInt |> Seq.toList) |> Array.toList
    let rec satisfies d c =
        match (d, c) with
        | (_, []) -> true
        | ([], _) -> false
        | (nextDigit :: tl1, nextPin :: tl2) -> if nextDigit = nextPin then satisfies d tl2 else satisfies tl1 c
    let rec checkNext n =
        let digits = n |> getDigits
        if constraints |> List.forall (fun c -> satisfies digits c) then n
        else checkNext (n + 1)
    checkNext 1000

// 40886
let problem80 () = 
    let isPerfectSquare n =
        let sq = int (Math.Sqrt (float n))
        sq * sq = n
    let getSum (n: int) =
        let mutable a = (bigint n) * (bigint 5)
        let mutable b = bigint 5
        for i in 1..2000 do
            if a >= b then
                a <- a - b
                b <- b + bigint 10
            else
                a <- a * bigint 100
                let (d, r) = (b / bigint 10, b % bigint 10)
                b <- d * bigint 100 + r
        b.ToString() |> Seq.map toInt |> Seq.take 100 |> Seq.sum
    [ 2..99 ] |> List.filter (fun n -> isPerfectSquare n = false) |> List.map getSum |> List.sum

// 427337
let problem81 () =
    let m = loadMatrixSep "P81Input.txt" ','
    let n = m.Length
    let grid = [| for i in 1..n -> [| for j in 1..n -> 0L |] |]
    grid.[0].[0] <- m.[0].[0]
    for i in 1..n do
        for j in 1..n do
            if i <> 1 || j <> 1 then
                let m1 = if i > 1 then grid.[i - 2].[j - 1] else Int64.MaxValue
                let m2 = if j > 1 then grid.[i - 1].[j - 2] else Int64.MaxValue
                grid.[i - 1].[j - 1] <- m.[i - 1].[j - 1] + (min m1 m2)
    grid.[n - 1].[n - 1]

// 260324
let problem82 () =
    let m = loadMatrixSep "P82Input.txt" ','
    let maxEl = 1000000000L
    let n = m.Length
    let grid = [| for i in 1..n -> [| for j in 1..n -> if j = 1 then m.[i - 1].[0] else maxEl |] |]
    for j in 2..n do
        let mutable keepLooping = true
        while keepLooping do
            let mutable hasChanges = false
            for i in 1..n do
                let m1 = if i > 1 then grid.[i - 2].[j - 1] else maxEl
                let m2 = if i < n then grid.[i].[j - 1] else maxEl
                let m3 = grid.[i - 1].[j - 2]
                let t = m.[i - 1].[j - 1] + min m1 (min m2 m3)
                if grid.[i - 1].[j - 1] > t then
                    hasChanges <- true
                    grid.[i - 1].[j - 1] <- t
            if hasChanges = false then keepLooping <- false
    grid |> Array.map (fun a -> a.[n - 1]) |> Array.min

// 425185
let problem83 () =
    let m = loadMatrixSep "P83Input.txt" ','
    let n = m.Length
    let getNeightbours (x, y) =
        let neightbour (hor, vert) = if x + hor >= 0 && x + hor < n && y + vert >= 0 && y + vert < n then Some(x + hor, y + vert) else None
        [(-1, 0); (1, 0); (0, -1); (0, 1)] |> List.choose neightbour
    let rec loop visited costs candidates =
        if candidates |> Set.count = 0 then costs |> Map.find (n - 1, n - 1)
        else
            let nextMin = Set.minElement candidates
            let newVisited = visited |> Set.add (snd nextMin)
            let newCosts = costs |> Map.add (snd nextMin) (fst nextMin)
            let neightbours = 
                getNeightbours (snd nextMin) 
                |> List.filter (fun t -> visited.Contains t = false)
                |> List.map (fun t -> (fst nextMin + m.[fst t].[snd t], t))
                |> Set.ofList
            let newCandidates = candidates |> Set.filter (fun (c, (i, j)) -> (i, j) <> snd nextMin) |> Set.union neightbours
            loop newVisited newCosts newCandidates
    let visited = Set.ofList [(0, 0)]
    let costs = Map.ofList [((0, 0), m.[0].[0])]
    let candidates = Set.ofList [(m.[0].[0] + m.[1].[0], (1, 0)); (m.[0].[0] + m.[0].[1], (0, 1))]
    loop visited costs candidates

// [ 10 ;  15 ;  24 ]
let problem84() = 
    let rand = new System.Random()
    let swap (a: _[]) x y =
        let tmp = a.[x]
        a.[x] <- a.[y]
        a.[y] <- tmp
    let shuffle a = Array.iteri (fun i _ -> swap a i (rand.Next(i, Array.length a))) a
    let visits = [| for i in 0..39 -> 0 |]
    let goto i = fun _ -> i
    let gotoR = fun i -> match i with
                            | _ when i >= 5  && i < 15 -> 15
                            | _ when i >= 15 && i < 25 -> 25
                            | _ when i >= 25 && i < 35 -> 35
                            | _ -> 5
    let gotoU = fun i -> if i >= 12 && i < 28 then 28 else 12
    let ccDeck = [ for i in 3..16 -> (fun (i: int) -> i) ] @ [ goto 0; goto 10; ]
    let chDeck = [ for i in 11..16 -> (fun (i: int) -> i) ] @ [
                    goto 0;
                    goto 10;
                    goto 11;
                    goto 24;
                    goto 39;
                    goto 5;
                    gotoR;
                    gotoR;
                    fun i -> if i >= 12 && i < 28 then 28 else 12
                    fun i -> (i + 37) % 40]
    let playDeck i deck = ((deck |> List.head) i, List.append (deck |> List.tail) [ deck |> List.head ])
    let rec sim pos ccDeck chDeck doubles i =
        visits.[pos] <- visits.[pos] + 1
        if i = 0 then
            visits |> Array.mapi (fun i k -> (-k, i)) |> Array.sortBy fst |> Seq.take 3 |> Seq.map snd |> Seq.toList
        else
            let roll1 = 1 + rand.Next 4
            let roll2 = 1 + rand.Next 4
            if roll1 = roll2 && doubles = 2 then 
                sim 10 ccDeck chDeck 0 (i - 1)
            else
                let pos = (pos + roll1 + roll2) % 40
                let (pos, chDeck) = if pos = 7 || pos = 22 || pos = 36 then playDeck pos chDeck else (pos, chDeck)
                let (pos, ccDeck) = if pos = 2 || pos = 17 || pos = 33 then playDeck pos ccDeck else (pos, ccDeck)
                let pos = if pos = 30 then 10 else pos
                sim pos ccDeck chDeck (if roll1 = roll2 then doubles + 1 else 0) (i - 1)
    sim 0 ccDeck chDeck 0 10000000

// 2772
let problem85 () =
    seq { for x in 1..2000 do
            for y in 1..2000 do
                let count = x * (x + 1) * y * (y + 1) / 4
                yield (Math.Abs(2000000 - count), x * y) } |> Seq.sort |> Seq.head |> snd

// 1818
let problem86 () =
    let rec loop m count =
        if count > 1000000 then m - 1
        else
            let s = seq { for s1 in 1..m do
                            for s2 in s1..m do
                                let lSquared = m * m + (s1 + s2) * (s1 + s2)
                                let l = Math.Sqrt(float lSquared) |> int
                                if l * l = lSquared then yield 1 } |> Seq.sum
            loop (m + 1) (count + s)
    loop 1 0

// 1097343
let problem87 () =
    let upperBound = 50000000L
    let primes = primeGenFast 10000 |> Array.map int64
    let squares = primes |> Array.map (fun n -> n * n)
    let cubes = primes |> Array.map (fun n -> n * n * n) |> Array.filter (fun n -> n < upperBound)
    let fourths = primes |> Array.map (fun n -> n * n * n * n) |> Array.filter (fun n -> n < upperBound)
    let nums = System.Collections.Generic.HashSet<int64>()
    for s1 in squares do
        if s1 < upperBound then
            for s2 in cubes do
                if s1 + s2 < upperBound then
                    for s3 in fourths do
                        if s1 + s2 + s3 < upperBound then nums.Add(s1 + s2 + s3) |> ignore
    nums.Count

// 7587457
let problem88 () =
    let k = 12000
    let upperBound = k * 2

    let inc factors = 
        let rec loop i = 
            if i >= Array.length factors then None
            else
                let nextFactor = factors.[i] + 1
                for j in 0..i do
                    factors.[j] <- nextFactor
                let prod = factors |> Array.fold (fun acc n -> acc * n) 1
                if prod <= upperBound then Some(factors)
                else loop (i + 1)
        loop 0

    let iterFactors f state = 
        let rec loop state factors = 
            let newState = f factors state
            match inc factors with
            | Some(e) -> loop newState e
            | _ ->
                if pow 2 (Array.length factors + 1) <= upperBound then
                    Array.create (Array.length factors + 1) 2 |> loop newState
                else
                    newState
        loop state [| 2; 2 |]

    let checkFactors factors acc =
        let prod = factors |> Array.fold (fun acc t -> acc * t) 1
        let sum = factors |> Array.sum
        if prod >= sum then
            let k = prod - sum + Array.length factors
            if Map.containsKey k acc = false || acc.[k] > prod then Map.add k prod acc
            else acc
        else acc

    iterFactors checkFactors Map.empty |> Map.toList |> List.filter (fun (t, _) -> t <= k) |> List.map snd |> Set.ofList |> Set.fold (fun acc t -> acc + t) 0

// 743
let problem89 () = 
    let impr (roman: String) = 
        let s = roman.Replace("VIIII", "IX").Replace("IIII", "IV").Replace("LXXXX", "XC").Replace("XXXX", "XL").Replace("VXXX", "XC").Replace("DCCCC", "CM").Replace("CCCC", "CD")
        roman.Length - s.Length
    getLines "P89Input.txt" |> Array.map impr |> Array.sum

// 1217
let problem90 () =
    let extend arr = 
        let (has6, has9) = arr |> List.fold (fun (has6, has9) n -> (has6 || n = 6, has9 || n = 9)) (false, false)
        if has6 && has9 = false then 9 :: arr
        else if has9 && has6 = false then 6 :: arr
        else arr

    let getSquareNums n = 
        let digits = getDigits (n * n)
        if digits.Tail.IsEmpty then (0, digits.Head) else (digits.Head, digits.Tail.Head)
    let squares = [ for n in 1..9 -> getSquareNums n ]
    let arrangments = choose [ 0..9 ] 6
    let extendedArrangments = arrangments |> List.map extend |> List.map Set.ofList
    let isGood arr1 arr2 = 
        let has d1 d2 = arr1 |> Set.contains d1 && arr2 |> Set.contains d2
        squares |> List.forall (fun (d1, d2) -> (has d1 d2 || has d2 d1))

    (seq { for a1 in extendedArrangments do
            for a2 in extendedArrangments do
                if isGood a1 a2 then yield 1 } |> Seq.length) / 2

// 14234
let problem91 () =
    let n = 50
    let origin = (0, 0)
    let points = seq { for p in 0..n do
                        for q in 0..n do
                            if p <> 0 || q <> 0 then yield (p, q) } |> Seq.toList
    let sideSquared (x1, y1) (x2, y2) = (x1 - x2) * (x1 - x2) + (y1 - y2) * (y1 - y2)
    (seq { for p in points do
            for q in points do
                if p <> q then
                    let (aSq, bSq, cSq) = (sideSquared p origin, sideSquared q origin, sideSquared p q)
                    if (aSq = bSq + cSq) || (bSq = aSq + cSq) || (cSq = aSq + bSq) then yield (p, q) } |> Seq.length) / 2

// 8581146
let problem92 () = 
    let rec next n = getDigits n |> List.map (fun n -> n * n) |> List.sum
    let rec arrivesAt89 n = 
        if n = 1 then false
        else if n = 89 then true
        else next n |> arrivesAt89
    seq { for n in 1..9999999 do if arrivesAt89 n then yield 1 } |> Seq.length

// 1258
let problem93 () = 
    let rec toFun op1 op2 op3 =
        [ fun d1 d2 d3 d4 -> op3 (op2 (op1 d1 d2) d3) d4;
          fun d1 d2 d3 d4 -> op3 (op1 d1 d2) (op2 d3 d4);
          fun d1 d2 d3 d4 -> op3 (op2 d1 (op1 d2 d3)) d4;
          fun d1 d2 d3 d4 -> op3 d1 (op2 (op1 d2 d3) d4);
          fun d1 d2 d3 d4 -> op3 d1 (op2 d2 (op1 d3 d4)); ]
    let binOps = [| (fun x y -> x + y); (fun x y -> x - y); (fun x y -> x * y); (fun x y -> if y <> 0.0 then x / y else Double.PositiveInfinity) |]
    let operations = seq { for o1 in binOps do for o2 in binOps do for o3 in binOps do yield toFun o1 o2 o3 } |> Seq.concat |> Seq.toList
    let getMax digits = 
        let perms = permutationsOf digits
        let allValues = 
            seq { for op in operations do
                    for d in perms do
                        let t = op d.Head d.Tail.Head d.Tail.Tail.Head d.Tail.Tail.Tail.Head
                        if t > 0.0 && t = float (int t) then yield int t } |> Set.ofSeq
        (Seq.initInfinite (fun n -> n + 1) |> Seq.skipWhile (fun n -> allValues.Contains n) |> Seq.head) - 1
    choose [0.0..9.0] 4 |> List.collect permutationsOf |> List.map (fun d -> (getMax d, d)) |> List.maxBy fst |> snd |> List.fold (fun acc n -> acc + ((int n).ToString())) ""

// 518408346
let problem94 () =
    let n = 3
    let (x1, y1) = (2, 1)
    let nextSln xk yk = (x1 * xk + n * y1 * yk, x1 * yk + y1 * xk)
    let allSln = 
        let rec loop (xk, yk) = 
            let a = ((2 * xk + 1) / 3, (2 * xk - 1) / 3)
            if (fst a) * 3 > 1000000000 then []
            else (xk, yk) :: loop (nextSln xk yk)
        loop (x1, y1)
    let isInt (f: float) = Math.Floor(f) = Math.Ceiling(f)
    seq { for (x, y) in allSln.Tail do
            let p a b area = if isInt a && isInt area then a * 2.0 + (a + b) else 0.0
            yield 
                p ((2.0 * float(x) + 1.0) / 3.0) 1.0 ((float x + 2.0) * float(y) / 3.0) +
                p ((2.0 * float(x) - 1.0) / 3.0) -1.0 ((float x - 2.0) * float(y) / 3.0)
            } |> Seq.sum

// 14316
let problem95 () = 
    let primes = primeGenFast 1000000 |> Array.map int64
    let chains = Array.init 1000000 (fun i -> if i = 0 then (1, 1) else (0, 0))

    let minChainEl n = 
        let rec loop n chain list =
            if fst chains.[n - 1] > 0 then chains.[n - 1]
            else 
                let divSum = int(getProperDivisorsSum (int64 n) primes)
                if divSum > 1000000 then (0, 0)
                else if Set.contains divSum chain then
                    let rec getMin list len minEl = 
                        match list with
                        | hd :: tl when hd = divSum -> (len, minEl)
                        | hd :: tl -> getMin tl (len + 1) (min minEl hd)
                        | _ -> raise (new Exception())
                    let answer = getMin list 1 divSum
                    for i in list do
                        chains.[i - 1] <- answer
                    answer
                else loop divSum (chain.Add divSum) (divSum :: list)
        (Set.ofList [ n ], [n]) ||> loop n
    [ 1..1000000 ] |> List.map minChainEl |> List.sortBy (fun t -> - fst t) |> List.head |> snd

type PuzzleState = Valid | Solved | Broken | HasAnswer

// 24702
let problem96 () = 
    let rec toPuzzles = function
        | hd :: tl -> 
            let puzzle = tl |> Seq.take 9 |> Seq.map (fun line -> line |> Seq.map toInt |> Seq.toArray) |> Seq.toArray
            puzzle :: (tl |> Seq.skip 9 |> Seq.toList |> toPuzzles)
        | _ -> []
    let puzzles = getLines "P96Input.txt" |> Array.toList |> toPuzzles

    let rowCells array row = seq { for i in 1..9 do yield Array.get (Array.get array (row - 1)) (i - 1) }
    let colCells array col = seq { for i in 1..9 do yield Array.get (Array.get array (i - 1)) (col - 1) }

    let rowVals puzzle i = rowCells puzzle i |> Seq.filter (fun el -> el > 0) |> Set.ofSeq
    let colVals puzzle i = colCells puzzle i |> Seq.filter (fun el -> el > 0) |> Set.ofSeq
    let self t = t
    let collectUniqueVals cells = cells |> Seq.choose self |> Seq.collect self |> Seq.groupBy self |> Seq.map (fun (k, v) -> (k, v |> Seq.length)) |> Seq.filter (fun (k, v) -> v = 1) |> Seq.map fst |> Set.ofSeq

    let valsForCube (puzzle: int[][]) i j = 
        let (x, y) = ((i - 1) / 3, (j - 1) / 3)
        seq { for i in 1 + x * 3..3 + x * 3 do 
                for j in 1 + y * 3..3 + y * 3 do
                    if puzzle.[i - 1].[j - 1] > 0 then yield puzzle.[i - 1].[j - 1] } |> Set.ofSeq

    let movesForCell (puzzle: int[][]) row col =
        if puzzle.[row - 1].[col - 1] > 0 then Some(Set.empty)
        else 
            let moves = Set.ofList [1..9] - (rowVals puzzle row) - (colVals puzzle col) - (valsForCube puzzle row col)
            if moves.Count > 0 then Some(moves) else None

    let movesFor (puzzle: int[][]) = 
        let moves = Array.init 9 (fun row -> Array.init 9 (fun col -> movesForCell puzzle (row + 1) (col + 1)))
        for row in 1..9 do
            let uniqueVals = rowCells moves row |> collectUniqueVals
            if uniqueVals.Count > 0 then
                for col in 1..9 do
                    if moves.[row - 1].[col - 1].IsSome then
                        let intr = Set.intersect moves.[row - 1].[col - 1].Value uniqueVals
                        if intr.Count = 1 then moves.[row - 1].[col - 1] <- Some(intr)
        for col in 1..9 do
            let uniqueVals = colCells moves col |> collectUniqueVals
            if uniqueVals.Count > 0 then
                for row in 1..9 do
                    if moves.[row - 1].[col - 1].IsSome then
                        let intr = Set.intersect moves.[row - 1].[col - 1].Value uniqueVals
                        if intr.Count = 1 then moves.[row - 1].[col - 1] <- Some(intr)
        moves

    let eval moves = 
        let mutable isValid = true
        let mutable isSolved = true
        for row in 0..8 do
            for col in 0..8 do
                match Array.get (Array.get moves row) col with
                | Some(s) -> if Set.count s > 0 then isSolved <- false
                | None -> isValid <- false
                | _ -> ()
        match (isValid, isSolved) with
        | (false, _) -> Broken
        | (_, true) -> Solved
        | (_, _) -> 
            if moves.[0].[0].Value.Count = 0 && moves.[0].[1].Value.Count = 0 && moves.[0].[2].Value.Count = 0 then HasAnswer
            else Valid

    let validate (puzzle: int[][]) = 
        let mutable isValid = true
        for row in 1..9 do
            let cells = rowCells puzzle row |> Seq.filter (fun n -> n > 0)
            if cells |> Set.ofSeq |> Set.count <> (cells |> Seq.length) then
                isValid <- false
        for col in 1..9 do
            let cells = colCells puzzle col |> Seq.filter (fun n -> n > 0)
            if cells |> Set.ofSeq |> Set.count <> (cells |> Seq.length) then
                isValid <- false
        isValid

    let rec solve puzzle = 
        let apply (puzzle: int[][]) moves = 
            let mutable hasChanges = false
            for row in 0..8 do
                for col in 0..8 do
                    match Array.get (Array.get moves row) col with
                    | Some(s) when Set.count s = 1 -> 
                        puzzle.[row].[col] <- s.MinimumElement
                        hasChanges <- true
                    | _ -> ()
            hasChanges
        if movesFor puzzle |> apply puzzle then solve puzzle else puzzle

    let guess puzzle = 
        let rec guesses puzzle = 
            seq {
                let simplified = solve puzzle
                if validate simplified then
                    let moves = movesFor simplified
                    match eval moves with
                    | Broken -> ()
                    | Solved -> yield puzzle
                    | _ ->
                        let (row, col, values) = 
                            seq { for row in 0..8 do
                                    for col in 0..8 do
                                        if moves.[row].[col].IsSome && moves.[row].[col].Value.Count = 2 then
                                            yield (row, col, moves.[row].[col].Value) } |> Seq.head
                        for v in values do
                            let copy = Array.init 9 (fun row -> Array.get puzzle row |> Array.copy)
                            copy.[row].[col] <- v
                            yield! guesses copy }
        guesses puzzle |> Seq.head

    let isCompleted (puzzle: int[][]) = puzzle.[0].[0] > 0 && puzzle.[0].[1] > 0 && puzzle.[0].[2] > 0

    let (solved, unsolved) = puzzles |> List.map (fun p -> solve p) |> List.partition isCompleted

    List.concat [ solved; unsolved |> List.map guess ] |> List.map (fun p -> p.[0].[0] * 100 + p.[0].[1] * 10 + p.[0].[2]) |> List.sum

// 8739992577
let problem97 () = 
    let s = (BigInteger.ModPow(bigint 2, bigint 7830457, bigint 100000000000L) * (bigint 28433) + bigint 1).ToString();
    s.Substring(s.Length - 10)

// 18769
let problem98 () =
    let line = getLines "P98Input.txt" |> Seq.head
    let words = line.Split ',' |> Seq.map (fun w -> w.Trim '"')
    let toStr a = new String(a |> Seq.toArray)
    let anagrams = 
        words 
        |> Seq.map (fun w -> (w |> Seq.sort |> toStr, w)) 
        |> Seq.groupBy fst 
        |> Seq.filter (fun (k, v) -> v |> Seq.length > 1) 
        |> Seq.map (fun (k, v) -> v |> Seq.map snd |> Seq.toList) 
        |> Seq.toList
    let squares = [ for i in 1..1000 -> i * i ] |> Set.ofList
    let digits = squares |> Seq.map getDigits |> Seq.toList

    let toInt ints = Seq.fold (fun acc n -> acc * 10 + n) 0 ints

    let maxSquareForSubst digits words =
        let digitsMap = Seq.zip (List.head words) digits |> Map.ofSeq
        let distinctDigits = Set.ofSeq digits
        if digitsMap.Count = distinctDigits.Count then
            let mappedWords = words |> List.map (fun word -> word |> Seq.map (fun c -> digitsMap.[c]))
            if mappedWords |> Seq.forall (fun s -> Seq.head s > 0) then
                let results = words |> List.map (fun word -> word |> Seq.map (fun c -> digitsMap.[c]) |> toInt)
                let notSquares = results |> List.filter (fun n -> squares.Contains n = false)
                if notSquares.Length = 0 then 
                    Some(results |> Seq.sortBy (fun n -> -n) |> Seq.head)
                else None
            else
                None
        else
            None

    let maxSquare words = 
        let possibleSubstitutions = digits |> List.filter (fun l -> List.head words |> String.length = List.length l)
        let squares = possibleSubstitutions |> List.choose (fun d -> maxSquareForSubst d words) |> List.sortBy (fun n -> -n)
        if squares.IsEmpty then 0 else squares.Head

    anagrams |> List.map maxSquare |> List.sortBy (fun n -> -n) |> List.head

// 709
let problem99 () = 
    getLines "P99Input.txt" 
    |> Array.mapi (fun i s -> (i + 1, s.Split ','))
    |> Array.map (fun (i, a) -> (i, float(toInt a.[0]), float(toInt a.[1])))
    |> Array.map (fun (i, x, y) -> (i, y * Math.Log10(x)))
    |> Array.sortBy (fun (i, v) -> -v)
    |> Seq.head
    |> fst

// 756872327473
let problem100 () =
    let next (b, n) = (3L * b + 2L * n - 2L, 4L * b + 3L * n - 3L)
    let rec loop (b, n) = 
        if n > 1000000000000L then b
        else next (b, n) |> loop 
    loop (15L, 21L)

[<EntryPoint>]
[<System.STAThread>]
let main argv =
    swStart ()
    let r = problem100 ()
    let t = swStop ()
    printfn "%s in %ims" (r.ToString()) t
    System.Windows.Clipboard.SetText (r.ToString())
    System.Console.ReadLine() |> ignore
    0