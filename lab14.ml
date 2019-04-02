(*
                             CS51 Lab 14
         Lazy Programming and Infinite Data Structures Part 1
 *)

(* This lab provides practice with delayed (lazy) computations, both
through user code and OCaml's built in Lazy module. In this lab, you
will use infinite data structures like streams.

======================================================================
Part 1: Programming with lazy streams

Recall the lazy stream type and associated functions from the reading,
here packaged up into a module. *)

module LazyStream =
  struct

    type 'a stream_internal = Cons of 'a * 'a stream
     and 'a stream = unit -> 'a stream_internal ;;

    (* Extracting the head and tail of a lazy stream *)
    let head (s : 'a stream) : 'a =
      let Cons (h, _t) = s () in h ;;

    let tail (s : 'a stream) : 'a stream =
      let Cons (_h, t) = s () in t ;;

    (* Extracting the first n elements of a stream into a list *)
    let rec first (n : int) (s : 'a stream) : 'a list =
      if n = 0 then []
      else head s :: first (n - 1) (tail s) ;;

    (* Mapping a function lazily over a stream *)
    let rec smap (f : 'a -> 'b) (s : 'a stream) : ('b stream) =
      fun () -> Cons (f (head s), smap f (tail s)) ;;

    (* Mapping a binary function over two streams *)
    let rec smap2 f s1 s2 =
      fun () -> Cons (f (head s1) (head s2),
                      smap2 f (tail s1) (tail s2)) ;;
  end ;;

open LazyStream ;;

(* Here, recalled from the reading, is the definition of an infinite
stream of ones. *)

let rec ones : int stream =
  fun () -> Cons (1, ones) ;;

(* Now you define some useful streams. Some of these were defined in
the reading, but see if you can come up with the definitions without
looking them up. *)

(*....................................................................
Exercise 1. An infinite stream of the integer 2. As usual, for this
and all succeeding exercises, you shouldn't feel beholden to how the
definition is introduced in the skeleton code below. (We'll stop
mentioning this now, and forevermore.)
....................................................................*)

let twos = smap (fun x -> x + 1) ones ;;

(*....................................................................
Exercise 2. An infinite stream of threes, built from the ones and
twos.
....................................................................*)

let threes = smap (fun x -> x + 1) twos ;;

(*....................................................................
Exercise 3. An infinite stream of natural numbers (0, 1, 2, 3, ...).
....................................................................*)

let rec nats = fun () -> Cons(0, smap succ nats) ;;

(*....................................................................
Exercise 4. Create a function zip_stream, which takes two streams and
'zips' them together; zip_stream should output one stream
created by alternating the elements of the two input streams.

For example, 'zipping' infinite streams of ones (1,1,1,1....) and
twos (2,2,2,2....) would look like this:

   let ones_twos = zip_stream ones twos;;
   -: val ones_twos : int stream = <fun>

   first 6 ones_twos;;
   -: int list = [1; 2; 1; 2; 1; 2]
....................................................................*)

let rec zip_stream (a : 'a stream) (b : 'a stream)
                   : 'a stream
               = fun () -> Cons(head a, fun () -> Cons(head b, zip_stream (tail a) (tail b)));;

(* Now some new examples. For these, you should build them from
previous streams (ones, twos, threes, nats) by making use of the
stream mapping functions (smap, smap2). *)

(*....................................................................
Exercise 5. Generate two infinite streams, one of the even natural
numbers, and one of the odds.
....................................................................*)

let evens = smap (fun x -> 2 * x) nats ;;
let odds = smap (fun x -> x + 1) evens ;;

(* In addition to mapping over streams, we should be able to use all
the other higher-order list functions you've grown to know and love,
like folding and filtering. So let's implement some. *)

(*....................................................................
Exercise 6. Define a function sfilter that takes a predicate (that is,
a function returning a bool) and a stream, and returns the stream that
contains all the elements in the argument stream that satisfy the
predicate. Here's an example -- generating a stream of even numbers by
filtering the natural numbers for the evens:

   # let evens = sfilter (fun x -> x mod 2 = 0) nats ;;
   val evens : int stream = <fun>
   # first 10 evens ;;
   - : int list = [0; 2; 4; 6; 8; 10; 12; 14; 16; 18]

Now define sfilter.
....................................................................*)

let rec sfilter (p : 'a -> bool) (s : 'a stream)
            : 'a stream
            = if p (head s) then fun () -> Cons(head s, sfilter p (tail s))
              else sfilter p (tail s) ;;

(*....................................................................
Exercise 7. Now redefine evens and odds (as evens2 and odds2) using
sfilter.
....................................................................*)

let evens2 = sfilter (fun x -> x mod 2 = 0) nats ;;
let odds2 = sfilter (fun x -> x mod 2 = 1) nats ;;

(*====================================================================
Part 2: Eratosthenes' Sieve

Eratosthenes' sieve is a method for generating the prime
numbers. Given a list (or stream) of natural numbers starting with 2,
we filter out those in the tail of the list not divisible by the head
of the list and then apply the sieve to that tail. The first few steps
go something like this: We start with the natural numbers (in the
example here, just a prefix of them).

2 3 4 5 6 7 8 9 10 11 12 13 14 15

The first element, 2, is prime. Now we remove numbers divisible by 2
from the tail of the list (marking here with a | the boundary between
the first element and the tail we're currently working on:

2  |  3 5 7 9 11 13 15

and apply the sieve to the tail:

2 3  |  5 7 11 13

and again:

2 3 5  |  7 11 13
2 3 5 7  |  11 13
...
2 3 5 7 11 13

Implement Eratosthenes sieve to generate an infinite stream of primes.
Example:

# primes = sieve (tail (tail nats)) ;;
# first 4 primes ;;
- : int list = [2; 3; 5; 7]

(You probably won't want to generate more than the first few primes
this way; it'll take too long. Here are some timings from the solution
code on my laptop:

  n      time for first n primes (seconds)
  1 --   0.00000691
  2 --   0.00002503
  3 --   0.00009799
  4 --   0.00133109
  5 --   0.00341392
  6 --   0.04702091
  7 --   0.18753004
  8 --   2.98919892

Just generating the first eight primes takes three seconds -- longer
if a less efficient sfilter is used.  You'll address this performance
problem in the next lab.) *)

(* In defining the sieve function, the following function may be
useful: *)

(* not_div_by n m -- Predicate determines if m is evenly divisible
   by n *)
let not_div_by (n : int) (m : int) : bool =
  not (m mod n = 0) ;;

let rec sieve s = fun () ->
                    Cons (head s,
                            sieve
                              (sfilter
                                  (fun x -> not_div_by (head s) x) (tail s))) ;;
