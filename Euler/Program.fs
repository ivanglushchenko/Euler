open Helpers
open System
open System.Collections

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
                let t = gcd64 p hd
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
    System.Numerics.BigInteger.Pow(bigint(2), 1000).ToString() |> Seq.map toInt |> Seq.sum

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
        else facRec (System.Numerics.BigInteger.Multiply(n, bigint(c))) (c - 1)
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
    let allPerutations = getPermutations s |> List.map (fun s -> s |> List.fold (fun acc i -> acc + i) "") |> List.sort
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
                    yield System.Numerics.BigInteger.Pow(bigint(a), b).ToString() } |> Set.ofSeq
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
        getPermutations digits
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
                        |> getPermutations 
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

[<EntryPoint>]
[<System.STAThread>]
let main argv =
    swStart ()
    let r = problem51 ()
    let t = swStop ()
    printfn "%s in %ims" (r.ToString()) t
    System.Windows.Clipboard.SetText (r.ToString())
    System.Console.ReadLine() |> ignore
    0