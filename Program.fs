open System;
let mutable possibleWords:seq<string> = System.IO.File.ReadAllLines("../../../dictionary.txt")
possibleWords <- Seq.filter (fun (a:string) -> a.Length > 2) possibleWords
let givenLetters = "tgaibspvr"//System.Console.ReadLine()
(*
Word Score Calculations
For most cases, the longest word is the best word, as increasing the length by 1 essentially doubles the score
Letters are summed = s
if the word length is greater than 3, the sum is multipled by 2**(l-3) where l is the length
2 points are added for rare words (negligible)
Occasionally, words spawn with extra points, which accounts for most error
*)
let LetterValue = Map( [('a',1); ('b',3); ('c',3); ('d',2); ('e',1); ('f',4); ('g',2); ('h',4); ('i',1); ('j',8); ('k',5); ('l',1); ('m',3); ('n',1); ('o',1); ('p',3); ('q',10); ('r',1); ('s',1); ('t',1); ('u',1); ('v',8); ('w',4); ('x',8); ('y',4); ('z',10);] )
let count x = Seq.filter ((=) x) >> Seq.length

let WordValue (word:string) = (Seq.fold (fun acc c -> acc + LetterValue[c]) 0 word) * (if word.Length > 3 then pown 2 (word.Length - 3) else 1)

let wordPossible (letters : string) (word:string) = Seq.forall (fun (c:char) -> letters.Contains(c) && (count c letters) >= (count c word) ) word

possibleWords <- Seq.filter (fun (word:string) -> wordPossible givenLetters word) possibleWords
possibleWords <- Seq.sortByDescending (fun (a:string) -> a.Length) possibleWords

//printfn "%d" (WordValue "deafer")
possibleWords |> Seq.iter (fun x -> printfn "%s " x)
let chooseBestWord words = Seq.maxBy (fun (a:string) -> WordValue a) words
let bestWord = chooseBestWord possibleWords
let useLetters letters used = Seq.fold (fun (acc: string) (a:char) -> if count a acc < (count a letters) - (count a used) then acc + string a else acc) "" letters
let remainingLetters = useLetters givenLetters bestWord
printfn "%s:%i" bestWord (WordValue bestWord)
remainingLetters |> Seq.iter (fun x -> printf "%c, " x)


let canFormByConcatenating word1 (word2:string) word3 = // We love chatgpt lul
    let word3Length = String.length word3

    let rec tryPrefix i =
        if i > word3Length then false
        else
            let prefix = word3.[0..i-1]
            let suffix = word3.Substring(i)
            let prefixCanBeFormed = Seq.fold (fun acc a -> acc && ((word1 |> count a) >= (prefix |> count a))) true prefix
            let suffixCanBeFormed = word2.StartsWith(suffix)
            if prefixCanBeFormed && suffixCanBeFormed then true
            else tryPrefix (i + 1)
    tryPrefix 1

let wordFits letters best word = (canFormByConcatenating letters best word) || (canFormByConcatenating letters ((Seq.rev best) |> String.Concat) ((Seq.rev word) |> String.Concat))

let wordsThatFit = Seq.filter (fun a -> wordFits remainingLetters bestWord a) possibleWords
let independentWords = Seq.filter (fun word -> wordPossible remainingLetters word) possibleWords
let secondWord = chooseBestWord (Seq.append wordsThatFit independentWords)
printfn "Second word: %s" secondWord

(*
For a word to be added after the best word is chosen it needs to contain a letter in `remaining letters`
2 possiblities
 - the remaining letters build a complete seperate word
 - the remaining letters can be used to build off the best word 

the only possible words able to be made must be present in the possible words list
*)