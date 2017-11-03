// If the numbers 1 to 5 are written out in words: one, two, three, four, five, then there are 3 + 3 + 5 + 4 + 4 = 19 letters used in total.
// If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

module Problem017 =
    let getFirstComponent lengthN (digits: int []) =
        match lengthN with
            | 6 -> digits.[0] * 100 + digits.[1] * 10 + digits.[2]
            | 5 -> digits.[0] * 10 + digits.[1]
            | 2 -> digits.[0] * 10
            | _ -> digits.[0]

    let getRemainder n lengthN firstComponent =
        match lengthN with
            | 3 -> n - firstComponent * 100
            | 2 -> n - firstComponent
            | _ -> n - firstComponent * 1000
            
    let getPlaceDescriptor placeValue =
        match placeValue with
            | 6 -> " thousand"
            | 5 -> " thousand"
            | 4 -> " thousand"
            | 3 -> " hundred"
            | _ -> ""

    // Outstanding: numbers like 100100 or 10100 will be printed as "one hundred thousand one hundred" and "ten thousand one hundred"
    // respectively - is this correct?  Or should it be, in these cases, "one hundred thousand AND one hundred"?
    let getConnector placeValue (digits: int []) =
        match placeValue with
            | 6 -> if (digits.[3] <> 0) then " " else " and "
            | 5 -> if (digits.[2] <> 0) then " " else " and "
            | 4 -> if (digits.[1] <> 0) then " " else " and "
            | 3 -> " and "
            | 2 -> "-"
            | _ -> ""

    let rec getPhoneticComponents n =
        if (n < 1) then raise (System.ArgumentException("printPhonetically currently does not support numbers less than 1."))
        if (n > 999999) then raise (System.ArgumentException("printPhonetically currently does not support numbers bigger than 999999"))

        seq {
            // This is not optimal - for long numbers, we'd be taking the tail end of the number to a char array, then re-casting
            // from string to int, over and over again - i.e. 49284029 becomes 4,9,2,8,4,0,2,9, then in the next iteration maybe
            // 2,8,4,0,2,9 (taking out both 4 and 9)... and so forth... which is inefficient.  But for now it's okay.
            let digits = n.ToString().ToCharArray() |> Array.map (string >> int)
            let lengthN = digits.Length

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
                let firstComponent = getFirstComponent lengthN digits
                let remainder = getRemainder n lengthN firstComponent

                yield! getPhoneticComponents firstComponent
                yield getPlaceDescriptor lengthN
                if (remainder <> 0) then yield getConnector lengthN digits
                if (remainder <> 0) then yield! getPhoneticComponents remainder
        }
        
    let printPhonetically n =
        getPhoneticComponents n |> Seq.fold (+) ""

    let concatenatedString = [1..1000] |> List.map printPhonetically |> List.fold (+) ""
    concatenatedString.Replace(" ", "").Replace("-", "").Length