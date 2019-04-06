(*
                             CS51 Lab 14
         Lazy Programming and Infinite Data Structures Part 1
 *)

(*
                               SOLUTION
 *)

(* This lab provides practice with delayed (lazy) computations, both
through user code and OCaml's built in Lazy module. In this lab, you
will use infinite data structures like streams.

======================================================================
Part 1: Programming with lazy streams

Recall the lazy stream type and associated functions from the reading,
here packaged up into a module. *)

(* An aside: The definitions here were chosen for simplicity, not for
   efficiency. For instance, the definitions of first, smap, and smap2
   all force their stream argument (that is, apply it to ()) more than
   once. The forces occur implicitly in the calls to head and tail. A
   more efficient implementation would force only once, saving the
   results. For instance,

    let rec smap (f : 'a -> 'b) (s : 'a stream) : ('b stream) =
      fun () ->
        let Cons (h, t) = s () in
        Cons (f h, smap f t) ;;

   Of course, in an implementation that uses memoizing thunks instead
   of unit functions to delay computation, this problem of redundancy
   of computation is eliminated. The first force of s (whichever call
   it arises from) causes the result to be cached and thereby made
   immmediately available for the second force. For this reason, we'll
   mostly ignore this issue of multiple forces in the solutions we
   give here, though we comment on it with alternative solutions
   below. *)

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

let rec twos =
  fun () -> Cons (2, twos) ;;

(*....................................................................
Exercise 2. An infinite stream of threes, built from the ones and
twos.
....................................................................*)

let threes =
  smap2 (+) ones twos ;;

(*....................................................................
Exercise 3. An infinite stream of natural numbers (0, 1, 2, 3, ...).
....................................................................*)


(* Here is the implementation from the textbook, which makes nats
   directly as a recursive *value*. *)

let rec nats =
  fun () -> Cons (0, smap ((+) 1) nats) ;;

(* An alternative implementation defines a recursive *function*
(nats_from) that generates the natural numbers starting from an
initial value.

    let rec nats_from (n : int) : int stream =
      fun () -> Cons (start, nats_from (n + 1)) ;;

    let nats : int stream = nats_from 0 ;;

This approach is faster but less "infinite data structure"-y. *)

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

let rec zip_stream (s1 : 'a stream) (s2 : 'a stream) : 'a stream =
  fun () -> Cons (head s1, zip_stream s2 (tail s1)) ;;

(* Now some new examples. For these, you should build them from
previous streams (ones, twos, threes, nats) by making use of the
stream mapping functions (smap, smap2). *)

(*....................................................................
Exercise 5. Generate two infinite streams, one of the even natural
numbers, and one of the odds.
....................................................................*)

(* There are several ways of generating a stream of evens. One is to
   implement the cyclic structure directly, as was done with nats
   above:

     let rec evens = fun () -> Cons (0, smap ((+) 2) evens) ;;

   But we asked for building them from existing streams, for instance,
   by mapping a doubling function over the nats:

     let evens = smap (( * ) 2) nats ;;

   or adding nats to itself:
 *)

let evens = smap2 (+) nats nats ;;

(* The odds stream has similar possibilities. Here we just add one to
   all the evens. *)

let odds = smap ((+) 1) evens ;;

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

(* The most straightforward way to implement filtering (though not
   very efficient) is as follows:

    let rec sfilter (pred : 'a -> bool) (s : 'a stream) : 'a stream =
      fun () ->
        if pred (head s) then Cons ((head s), sfilter pred (tail s))
        else (sfilter pred (tail s)) () ;;

   There are multiple alternatives. Perhaps you implemented one of
   these.

   The definition above forces evaluation of s (that is, applies s to
   ()) several times (on every call of head and tail). This is the
   same issue mentioned above in the discussion of smap. To remove
   this expensive recomputation, we can do the force once and store
   the result, using the parts as needed:

    let rec sfilter (pred : 'a -> bool) (s : 'a stream) : 'a stream =
      fun () ->
        let Cons (h, t) = s () in
        if pred h then Cons (h, sfilter pred t)
        else (sfilter pred t) () ;;

   This is the version we use below.

   Another set of variations involve moving the test of the head
   outside the "thunk". In that case, sfilter verifies the pred
   condition on the head of the stream *before* doing any postponement
   of the tail. It thus eagerly filters out non-pred elements,
   postponing only at the first pred-satisfying element. We get:

    let rec sfilter (pred : 'a -> bool) (s : 'a stream) : 'a stream =
      if pred (head s)
      then fun () -> Cons (head s, sfilter pred (tail s))
      else (sfilter pred (tail s)) ;;

   This version can run *much* slower than the one above. Indeed, on a
   stream none of whose elements satisfy p, this sfilter will never
   return, because the call to sfilter in the else clause isn't
   postponed at all! That problem can be remedied by delaying the else
   branch as well:

    let rec sfilter (pred : 'a -> bool) (s : 'a stream) : 'a stream =
      if pred (head s)
      then fun () -> Cons (head s, sfilter pred (tail s))
      else fun () -> (sfilter pred (tail s)) () ;;

   Again, these implementations implicitly re-force s multiple times
   by virtue of the multiple calls to head and tail. Instead, we can
   force s explicitly once and reuse the results.

    let rec sfilter pred s =
      let Cons (h, t) = s () in
      if pred h then fun () -> Cons (h, sfilter pred t)
      else fun () -> (sfilter pred t) () ;;
 *)

let rec sfilter (pred : 'a -> bool) (s : 'a stream) : 'a stream =
  fun () ->
    let Cons (h, t) = s () in
    if pred h then Cons (h, sfilter pred t)
    else (sfilter pred t) () ;;

(*....................................................................
Exercise 7. Now redefine evens and odds (as evens2 and odds2) using
sfilter.
....................................................................*)

let even x = (x mod 2) = 0 ;;
let odd x = not (even x) ;;

let evens2 = sfilter even nats ;;
let odds2 = sfilter odd nats ;;

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

(* The idea in implementing sieve is as follows:

   1. Retrieve the head and tail of the stream. The head is the first
      prime in the result stream; the tail is the list of remaining
      elements that have not been sieved yet. For instance,

      head      | tail
      2         | 3 4 5 6 7 8 9 10 11 ...

   2. Filter out all multiples of the head from the tail.

      head      | filtered tail
      2         | 3 5 7 9 11 ...

   3. Sieve the filtered tail to generate all primes starting with the
      first element of the tail.

      head      | sieved filtered tail
      2         | 3 5 7 11 ...

   4. Add the head on the front of the sieved results.

      2 3 5 7 11 ...

   5. Of course, this whole series of computations should be delayed,
      and only executed when forced to do so.

A direct implementation of this idea (with numbers keyed to the
description above) would be:

let rec sieve s =
  fun () -> Cons (head s, sieve (sfilter (not_div_by (head s)) (tail s))) ;;
    ^         ^     ^        ^      ^                              ^
    |         |     |        |      |                              |
    5         4     1        3      2                              1

But as in sfilter above this forces s multiple times. Instead, we can
force once and save the results:

let rec sieve s =
  fun () ->
    let Cons (h, t) = s () in
    Cons (h, sieve (sfilter (not_div_by h) t)) ;;

Finally, the force of s can be done before delaying the rest of the
computation or after, as we do below. (Either way requires essentially
the same amount of time.) *)

let rec sieve (s : int stream) : int stream =
  let Cons (h, t) = s () in
  fun () -> Cons (h, sieve (sfilter (not_div_by h) t)) ;;

(* With the sieve function in hand, we can generate an infinite stream
   of primes. *)

let primes : int stream = sieve (tail (tail nats)) ;;

(* We generate a table of some times to generate primes, stopping as
soon as the next prime takes more than half a second: *)

exception Done ;;

let prime_timing () =
  try
    print_endline "Testing sieve based on lazy streams";
    for n = 1 to 100 do
      let _l, t = CS51.call_timed (first n) primes in
      Printf.printf "%3d -- %12.8f\n" n t;
      if t > 0.5 then raise Done
    done
  with Done -> () ;;

let _ = prime_timing () ;;
