// If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
// If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

module Problem017 =
    let getPlaceDescriptor placeValue =
        match placeValue with
            | 4 -> " thousand"
            | 3 -> " hundred"
            | _ -> ""

    let getConnector placeValue number =
        match placeValue with
            | 4 -> if ((number/100)%10 <> 0) then " " else " and "
            | 3 -> " and "
            | 2 -> "-"
            | _ -> ""

    let rec getPhoneticComponents n =
        if (n > 9999) then raise (System.ArgumentException("printPhonetically currently does not support numbers bigger than 9999"))

        seq {
            let lengthN = n.ToString().Length
            let firstDigit = n/(pown 10 (lengthN-1))
            let remainingDigits = (n - firstDigit * (pown 10 (lengthN-1)))

            match n with
            | 1 -> yield "one"
            | 2 -> yield "two"
            | 3 -> yield "three"
            | 4 -> yield "four"
            | 5 -> yield "five"
            | 6 -> yield "six"
            | 7 -> yield "seven"
            | 8 -> yield "eight"
            | 9 -> yield "nine"
            | 10 -> yield "ten"
            | 11 -> yield "eleven"
            | 12 -> yield "twelve"
            | 13 -> yield "thirteen"
            | 14 -> yield "fourteen"
            | 15 -> yield "fifteen"
            | 16 -> yield "sixteen"
            | 17 -> yield "seventeen"
            | 18 -> yield "eighteen"
            | 19 -> yield "nineteen"
            | 20 -> yield "twenty"
            | 30 -> yield "thirty"
            | 40 -> yield "forty"
            | 50 -> yield "fifty"
            | 60 -> yield "sixty"
            | 70 -> yield "seventy"
            | 80 -> yield "eighty"
            | 90 -> yield "ninety"
            | _ ->
                if (n<100) then yield! getPhoneticComponents (n-(n%10))
                if (n>= 100) then yield! getPhoneticComponents firstDigit
                yield getPlaceDescriptor lengthN
                if (remainingDigits <> 0) then yield getConnector lengthN n
                if (remainingDigits <> 0) then yield! getPhoneticComponents remainingDigits
        }
    let printPhonetically n =
        getPhoneticComponents n |> Seq.fold (+) ""

    let concatenatedString = [1..1000] |> List.map printPhonetically |> List.fold (+) ""
    concatenatedString.Replace(" ", "").Replace("-", "").Length