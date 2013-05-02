module LuhnCheck

open System
open System.IO


let asciiZero = int '0'

let DoubleDigit digit =
    let doubled = digit * 2
    let remainder = doubled % 10
    
    match doubled with
    | (x) when x >= 10 -> 1 + remainder
    | _ -> doubled    


let CheckDoubleDigit x =
    match x with
    | (0, 0) -> true
    | (0, _) -> false
    | (_, 0) -> false
    | _ -> true


let FilterDigit(length, index, digit) =    
    match CheckDoubleDigit(length % 2, index % 2) with
    | true -> DoubleDigit digit
    | _ -> digit


let FilterCardNumber cardNumber =
    let length = String.length cardNumber
    [ for i in 0 .. length - 1 -> FilterDigit(length, i, int cardNumber.[i] - asciiZero) ]


let Check filtered =
    match List.sum filtered % 10 with
    | 0 -> true
    | _ -> false   
    
    
let ReplaceWithX x =
    match x with
    | (ch) when ch >= '0' && ch <= '9' -> "X"
    | _ -> string x  
    

let FormatResult(cardNumber, isValid) =
    match isValid with
    | true -> String.collect ReplaceWithX cardNumber
    | _ -> cardNumber


[<EntryPoint>]
let main args = 
    let stream = new StreamReader(Console.OpenStandardInput())
    while not stream.EndOfStream do
        let input = stream.ReadLine()
        let cardNumber = String.Concat(Seq.filter (fun x -> x >= '0' && x <= '9') input)
        let digits = cardNumber.Length

        match digits with
        | 14 | 15 | 16 ->        
            stdout.WriteLine(FormatResult(input, Check (FilterCardNumber cardNumber)))
        | _ -> stdout.WriteLine(input)
    0
