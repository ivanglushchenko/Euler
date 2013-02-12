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
    let m = [|
        [| 08; 02; 22; 97; 38; 15; 00; 40; 00; 75; 04; 05; 07; 78; 52; 12; 50; 77; 91; 08 |]
        [| 49; 49; 99; 40; 17; 81; 18; 57; 60; 87; 17; 40; 98; 43; 69; 48; 04; 56; 62; 00 |]
        [| 81; 49; 31; 73; 55; 79; 14; 29; 93; 71; 40; 67; 53; 88; 30; 03; 49; 13; 36; 65 |]
        [| 52; 70; 95; 23; 04; 60; 11; 42; 69; 24; 68; 56; 01; 32; 56; 71; 37; 02; 36; 91 |]
        [| 22; 31; 16; 71; 51; 67; 63; 89; 41; 92; 36; 54; 22; 40; 40; 28; 66; 33; 13; 80 |]
        [| 24; 47; 32; 60; 99; 03; 45; 02; 44; 75; 33; 53; 78; 36; 84; 20; 35; 17; 12; 50 |]
        [| 32; 98; 81; 28; 64; 23; 67; 10; 26; 38; 40; 67; 59; 54; 70; 66; 18; 38; 64; 70 |]
        [| 67; 26; 20; 68; 02; 62; 12; 20; 95; 63; 94; 39; 63; 08; 40; 91; 66; 49; 94; 21 |]
        [| 24; 55; 58; 05; 66; 73; 99; 26; 97; 17; 78; 78; 96; 83; 14; 88; 34; 89; 63; 72 |]
        [| 21; 36; 23; 09; 75; 00; 76; 44; 20; 45; 35; 14; 00; 61; 33; 97; 34; 31; 33; 95 |]
        [| 78; 17; 53; 28; 22; 75; 31; 67; 15; 94; 03; 80; 04; 62; 16; 14; 09; 53; 56; 92 |]
        [| 16; 39; 05; 42; 96; 35; 31; 47; 55; 58; 88; 24; 00; 17; 54; 24; 36; 29; 85; 57 |]
        [| 86; 56; 00; 48; 35; 71; 89; 07; 05; 44; 44; 37; 44; 60; 21; 58; 51; 54; 17; 58 |]
        [| 19; 80; 81; 68; 05; 94; 47; 69; 28; 73; 92; 13; 86; 52; 17; 77; 04; 89; 55; 40 |]
        [| 04; 52; 08; 83; 97; 35; 99; 16; 07; 97; 57; 32; 16; 26; 26; 79; 33; 27; 98; 66 |]
        [| 88; 36; 68; 87; 57; 62; 20; 72; 03; 46; 33; 67; 46; 55; 12; 32; 63; 93; 53; 69 |]
        [| 04; 42; 16; 73; 38; 25; 39; 11; 24; 94; 72; 18; 08; 46; 29; 32; 40; 62; 76; 36 |]
        [| 20; 69; 36; 41; 72; 30; 23; 88; 34; 62; 99; 69; 82; 67; 59; 85; 74; 04; 36; 16 |]
        [| 20; 73; 35; 29; 78; 31; 90; 01; 74; 31; 49; 71; 48; 86; 81; 16; 23; 57; 05; 54 |]
        [| 01; 70; 54; 71; 83; 51; 54; 69; 16; 92; 33; 48; 61; 43; 52; 01; 89; 19; 67; 48 |] |]
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
    let nums = [|
        bigint.Parse("37107287533902102798797998220837590246510135740250");
        bigint.Parse("46376937677490009712648124896970078050417018260538");
        bigint.Parse("74324986199524741059474233309513058123726617309629");
        bigint.Parse("91942213363574161572522430563301811072406154908250");
        bigint.Parse("23067588207539346171171980310421047513778063246676");
        bigint.Parse("89261670696623633820136378418383684178734361726757");
        bigint.Parse("28112879812849979408065481931592621691275889832738");
        bigint.Parse("44274228917432520321923589422876796487670272189318");
        bigint.Parse("47451445736001306439091167216856844588711603153276");
        bigint.Parse("70386486105843025439939619828917593665686757934951");
        bigint.Parse("62176457141856560629502157223196586755079324193331");
        bigint.Parse("64906352462741904929101432445813822663347944758178");
        bigint.Parse("92575867718337217661963751590579239728245598838407");
        bigint.Parse("58203565325359399008402633568948830189458628227828");
        bigint.Parse("80181199384826282014278194139940567587151170094390");
        bigint.Parse("35398664372827112653829987240784473053190104293586");
        bigint.Parse("86515506006295864861532075273371959191420517255829");
        bigint.Parse("71693888707715466499115593487603532921714970056938");
        bigint.Parse("54370070576826684624621495650076471787294438377604");
        bigint.Parse("53282654108756828443191190634694037855217779295145");
        bigint.Parse("36123272525000296071075082563815656710885258350721");
        bigint.Parse("45876576172410976447339110607218265236877223636045");
        bigint.Parse("17423706905851860660448207621209813287860733969412");
        bigint.Parse("81142660418086830619328460811191061556940512689692");
        bigint.Parse("51934325451728388641918047049293215058642563049483");
        bigint.Parse("62467221648435076201727918039944693004732956340691");
        bigint.Parse("15732444386908125794514089057706229429197107928209");
        bigint.Parse("55037687525678773091862540744969844508330393682126");
        bigint.Parse("18336384825330154686196124348767681297534375946515");
        bigint.Parse("80386287592878490201521685554828717201219257766954");
        bigint.Parse("78182833757993103614740356856449095527097864797581");
        bigint.Parse("16726320100436897842553539920931837441497806860984");
        bigint.Parse("48403098129077791799088218795327364475675590848030");
        bigint.Parse("87086987551392711854517078544161852424320693150332");
        bigint.Parse("59959406895756536782107074926966537676326235447210");
        bigint.Parse("69793950679652694742597709739166693763042633987085");
        bigint.Parse("41052684708299085211399427365734116182760315001271");
        bigint.Parse("65378607361501080857009149939512557028198746004375");
        bigint.Parse("35829035317434717326932123578154982629742552737307");
        bigint.Parse("94953759765105305946966067683156574377167401875275");
        bigint.Parse("88902802571733229619176668713819931811048770190271");
        bigint.Parse("25267680276078003013678680992525463401061632866526");
        bigint.Parse("36270218540497705585629946580636237993140746255962");
        bigint.Parse("24074486908231174977792365466257246923322810917141");
        bigint.Parse("91430288197103288597806669760892938638285025333403");
        bigint.Parse("34413065578016127815921815005561868836468420090470");
        bigint.Parse("23053081172816430487623791969842487255036638784583");
        bigint.Parse("11487696932154902810424020138335124462181441773470");
        bigint.Parse("63783299490636259666498587618221225225512486764533");
        bigint.Parse("67720186971698544312419572409913959008952310058822");
        bigint.Parse("95548255300263520781532296796249481641953868218774");
        bigint.Parse("76085327132285723110424803456124867697064507995236");
        bigint.Parse("37774242535411291684276865538926205024910326572967");
        bigint.Parse("23701913275725675285653248258265463092207058596522");
        bigint.Parse("29798860272258331913126375147341994889534765745501");
        bigint.Parse("18495701454879288984856827726077713721403798879715");
        bigint.Parse("38298203783031473527721580348144513491373226651381");
        bigint.Parse("34829543829199918180278916522431027392251122869539");
        bigint.Parse("40957953066405232632538044100059654939159879593635");
        bigint.Parse("29746152185502371307642255121183693803580388584903");
        bigint.Parse("41698116222072977186158236678424689157993532961922");
        bigint.Parse("62467957194401269043877107275048102390895523597457");
        bigint.Parse("23189706772547915061505504953922979530901129967519");
        bigint.Parse("86188088225875314529584099251203829009407770775672");
        bigint.Parse("11306739708304724483816533873502340845647058077308");
        bigint.Parse("82959174767140363198008187129011875491310547126581");
        bigint.Parse("97623331044818386269515456334926366572897563400500");
        bigint.Parse("42846280183517070527831839425882145521227251250327");
        bigint.Parse("55121603546981200581762165212827652751691296897789");
        bigint.Parse("32238195734329339946437501907836945765883352399886");
        bigint.Parse("75506164965184775180738168837861091527357929701337");
        bigint.Parse("62177842752192623401942399639168044983993173312731");
        bigint.Parse("32924185707147349566916674687634660915035914677504");
        bigint.Parse("99518671430235219628894890102423325116913619626622");
        bigint.Parse("73267460800591547471830798392868535206946944540724");
        bigint.Parse("76841822524674417161514036427982273348055556214818");
        bigint.Parse("97142617910342598647204516893989422179826088076852");
        bigint.Parse("87783646182799346313767754307809363333018982642090");
        bigint.Parse("10848802521674670883215120185883543223812876952786");
        bigint.Parse("71329612474782464538636993009049310363619763878039");
        bigint.Parse("62184073572399794223406235393808339651327408011116");
        bigint.Parse("66627891981488087797941876876144230030984490851411");
        bigint.Parse("60661826293682836764744779239180335110989069790714");
        bigint.Parse("85786944089552990653640447425576083659976645795096");
        bigint.Parse("66024396409905389607120198219976047599490197230297");
        bigint.Parse("64913982680032973156037120041377903785566085089252");
        bigint.Parse("16730939319872750275468906903707539413042652315011");
        bigint.Parse("94809377245048795150954100921645863754710598436791");
        bigint.Parse("78639167021187492431995700641917969777599028300699");
        bigint.Parse("15368713711936614952811305876380278410754449733078");
        bigint.Parse("40789923115535562561142322423255033685442488917353");
        bigint.Parse("44889911501440648020369068063960672322193204149535");
        bigint.Parse("41503128880339536053299340368006977710650566631954");
        bigint.Parse("81234880673210146739058568557934581403627822703280");
        bigint.Parse("82616570773948327592232845941706525094512325230608");
        bigint.Parse("22918802058777319719839450180888072429661980811197");
        bigint.Parse("77158542502016545090413245809786882778948721859617");
        bigint.Parse("72107838435069186155435662884062257473692284509516");
        bigint.Parse("20849603980134001723930671666823555245252804609722");
        bigint.Parse("53503534226472524250874054075591789781264330331690"); |]
    nums |> Array.sum

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
    let lines = getLines "P18Input.txt" |> Array.map (fun s -> s.Split ' ' |> Array.map (fun t -> System.Int64.Parse t))
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

[<EntryPoint>]
let main argv = 
    swStart ()
    let r = problem24 ()
    let t = swStop ()
    printfn "%s in %ims" (r.ToString()) t
    System.Console.ReadLine() |> ignore
    0
