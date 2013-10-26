open System

/// Constructs a list. This helps with my lamdbda-free
/// style since the :: operator can't be called as a function.
let cons (head:'t) (tail:'t list) = head::tail

// Gets the value of a digit. Can't cast directly from char
// to int since that would give the character code.
let digitValue (digit:char) = digit |> string |> int

/// Returns the cartesian product of two sequences.
let cross (xs:'x seq) (ys:'y seq) = 
    seq { for x in xs do for y in ys do yield x, y } 

/// Replaces a single character in a string and returns the new string.
let replace (replacement:char) (input:string) (index:int) =
    let chars = input.ToCharArray()
    chars.[index] <- replacement
    String chars

/// Finds a substring of digits and separators with the given 
/// digitLength at the given startIndex, returning a list of 
/// index and digit tuples if there was a match.
let rec find (input:string) (index:int, digitLength:int) =
    if digitLength = 0 then Some [] else
    if index = input.Length then None else
    match input.[index] with
    | digit when Char.IsDigit digit -> 
        find input (index + 1, digitLength - 1) 
        |> Option.map (cons (index, digitValue digit))
    | '-' | ' ' -> 
        find input (index + 1, digitLength)
    | _ -> None

/// Performs Luhn check on a sequence of digits.
let luhn (digits:int seq) =
    let doubleIfOdd i x = if i % 2 = 1 then 2 * x else x 
    let sumDigits x = x / 10 + x % 10
    let divisibleBy d x = x % d = 0
    digits
    |> Seq.toList
    |> List.rev
    |> Seq.mapi doubleIfOdd
    |> Seq.sumBy sumDigits
    |> divisibleBy 10

/// Masks all instances of credit card numbers in the given string.
let mask (input:string) =
    cross {0 .. input.Length - 1} {14 .. 16}
    |> Seq.choose (find input)
    |> Seq.filter (luhn << Seq.map snd)
    |> Seq.concat
    |> Seq.map fst
    |> Seq.fold (replace 'X') input

/// Main loop.
while stdin.Peek() <> -1 do
    stdin.ReadLine()
    |> mask
    |> printf "%s\n"
