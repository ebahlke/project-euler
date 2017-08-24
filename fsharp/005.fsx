// PROBLEM 5:
// 2520 is the smallest number that can be divided by each of the numbers from 1 to 10 without any remainder.
// What is the smallest positive number that is evenly divisible by all of the numbers from 1 to 20?

module Problem005
    let rec gcd a b =
        if b = 0 then a
        else gcd b (a%b)

    let rec gcd' a b =
        if b = bigint(0) then a
        else gcd' b (a%b)

    let lcm a b =
        a*b/(gcd a b)

    let lcm' (a:bigint) (b:bigint) =
        a*b/(gcd' a b)
    
    let listlcm (numbers: list<int>) =
        if numbers.Length < 2 then raise (System.ArgumentException("listlcm is designed to be used with lists of at least and ideally more than two numbers."))
        List.fold lcm 1 numbers

    let listlcm' (numbers: list<bigint>) =
        if numbers.Length < 2 then raise (System.ArgumentException("listlcm is designed to be used with lists of at least and ideally more than two numbers."))
        List.fold lcm' (bigint(1)) numbers

    let rec range (rStart:bigint) (rEnd:bigint) =
        if rStart = rEnd then [rStart]
        else rStart :: (range (rStart+bigint(1)) rEnd)