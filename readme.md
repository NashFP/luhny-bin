NashFP polyglot "The Luhny Bin" excercise
=========================================

This coding challenge is based on this post by Bob Lee http://corner.squareup.com/2011/11/luhny-bin.html

Excerpt:

Mistakes happen. At Square, we accept that human error is inevitable. We anticipate potential slip-ups and implement safety measures to mitigate—and oftentimes completely eliminate—any repercussions.

For example, Square’s Luhn filter monitors logs and masks anything that looks like a credit card number. If a number like “4111 1111 1111 1111” were accidentally logged as part of an error message, our filter would replace it with “XXXX XXXX XXXX XXXX” and page an on call engineer.

The Luhn filter looks for sequences of digits that pass the Luhn check, a simple checksum algorithm invented by Hans Peter Luhn in 1954. All valid credit card numbers pass the Luhn check, thereby enabling computer programs, like our log filter, to distinguish credit card numbers from random digit sequences.

The Luhn check works like this:

Starting from the rightmost digit and working left, double every second digit.
If a product has two digits, treat the digits independently.
Sum each individual digit, including the non-doubled digits.
Divide the result by 10.
If the remainder is 0, the number passed the Luhn check.
For example, “5678” passes the Luhn check:

Double every other digit: 10, 6, 14, 8
Sum the individual digits: (1 + 0) + 6 + (1 + 4) + 8 = 20
Divide the result by 10: 20 mod 10 = 0 Pass
“6789” does not:

Double every other digit: 12, 7, 16, 9
Sum the individual digits: (1 + 2) + 7 + (1 + 6) + 9 = 26
Divide the result by 10: 26 mod 10 != 0 Fail
Now for the challenge…
Write a command line program that reads ASCII text from standard input, masks sequences of digits that look like credit card numbers, and writes the filtered text to standard output. For the purposes of this challenge, a credit card number:

Consists of digits, spaces (' ') and hyphens ('-').
Has between 14 and 16 digits, inclusive.
Passes the Luhn check.
If a sequence of digits looks like a credit card number, replace each digit with an 'X'. Any characters, including digits, may flank a credit card number. Beware. Potential credit card numbers can overlap. A valid 16-digit number can even contain a valid 14 or 15-digit number. Your program must mask every digit.

Contribute your solution by adding a folder name {your twitter handle}+{your language} such as "bryan_hunter+erlang".

Enjoy.