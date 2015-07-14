To install the monolith executable:

    cabal configure --enable-tests && cabal build

To install a standalone executable for problem `X`:

    make NUM=X && make clean

Use the monolithic executable:

    you@box: euler Description 1
    Just: 
    If we list all the natural numbers below 10 that are multiples of 3 or 5, 
    we get 3, 5, 6 and 9. The sum of these multiples is 23.
    
    Find the sum of all the multiples of 3 or 5 below 1000.

    you@box: euler Answer 1
    Just: 233168
    you@box: euler Answer 999
    Nothing
    you@box: euler Solution 1 --help
      Usage: euler <limit> <divisors>
      Calculates the sum of all natural numbers less then <limit>
      and also divisible by a number in <divisors>
        <limit>    1000  ::  Integer
        <divisors> [3,5] :: [Integer]

    you@box: euler Solution 1 10000000000 [4,9,11,13,25,49]
    23694876572981021759



