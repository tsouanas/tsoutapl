[:
 : Sample program file for the 'typed' system.
 :
 : Changes from the untyped include:
 : - You must use typed lambda abstractions, e.g.:
 :   (\x:Bool. x) true;
 : - wrong terms have been added to the syntax, e.g.:
 :   wrong "what went wrong";
 : - New syntactic sugar:
 :   assert (condition) "failure message";
 :   desugars to...
 :   if condition then unit else wrong "failure message";
 :]

assert (iszero 2) "2 must be zero";
assert (true) "this won't be shown";

(\x:Nat. x) n;
(w w);
(\p:(Nat -> Bool). p 3) (\c:Nat -> Bool. true);


wrong "Ooops!  Something went wrong.";  % like this one

if iszero 0 then 42 else wrong "should never reach this";
