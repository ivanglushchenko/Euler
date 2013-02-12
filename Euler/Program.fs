open Helpers

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
    let names = 
        getLines "P22Input.txt" 
        |> Array.collect (fun l -> l.Split ',') 
        |> Array.sort
        |> Array.map (fun n -> n.Trim '"' |> Seq.toList)
    let getNameScore s =
        let rec loop (s: char list) acc = 
            match s with
            | hd :: tl -> loop tl (acc + System.Convert.ToInt32 hd - System.Convert.ToInt32 'A' + 1)
            | _        -> acc
        loop s 0
    names |> Array.mapi (fun i n -> (i + 1) * (getNameScore n)) |> Array.sum

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
    let getSplits xs = 
        let rec loop before after =
            match after with
            | hd :: tl -> (before, after) :: (loop (before @ [after.Head]) after.Tail)
            | []       -> [(before, [])]
        loop [] xs
    let extend el xs = getSplits xs |> List.map (fun (before, after) -> before @ [el] @ after)
    let rec getPermutations set = 
        match set with
        | el :: nk :: tl ->
            let rest = getPermutations (nk :: tl)
            rest |> List.collect (fun s -> extend el s)
        | hd :: []       -> [ [ hd ] ]
        | []             -> []
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

let problem27 () =
    let f n a b = n * n + a * n + b
    let primes = getAllPrimes 10000L
    let choicesForB = [ for x in primes |> List.filter (fun p -> p < 1000L) -> [ x; -x ] ] |> List.concat
    let coefs =
        seq { for a in -999L..999L do
                for b in choicesForB do yield (a, b) }
    //let getMaxNumOfPrimes 
    //coefs |> 
    0

[<EntryPoint>]
[<System.STAThread>]
let main argv =
    swStart ()
    let r = problem27 ()
    let t = swStop ()
    printfn "%s in %ims" (r.ToString()) t
    System.Windows.Clipboard.SetText (r.ToString())
    System.Console.ReadLine() |> ignore
    0
