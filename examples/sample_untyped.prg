[:
 : Sample program file for the untyped system.
 :
 :    Program file format:
 :    - Write one term per line.
 :    - Terminate each line with a semicolon.
 :    - Variable names begin with a lower-case letter.
 :    - Application is typed like so: s t;
 :    - Untyped lambda abstractions are typed like so: (\f. (f x))
 :    - Typed lambda abstractions are typed like so: (\f:X->Bool. (f x))
 :    - You do not have to use zero and (succ (succ (succ ...)...).
 :      Instead, you can use actual numbers.
 :    - unit, iszero ..., if ... then ... else ..., succ ...,
 :      pred ..., etc., all work as expected.
 :    - Multiline comments are typed within [: and :] and can be nested.
 :    - Single-line comments begin with %.
 :]

unit;

((\y. (y (\y. y))) z);
((\x. (\z. (y z))) x);
((\x. (\z. (y x))) z);
(true 3);
(true (\x. y));

(\x. x);

((\x. x) 5);
0;

(\x. (x x));

if (iszero true) then 42 else ((\x. x) 66);

if (iszero 10) then 42 else ((\x. x) 66);

iszero true;

[: WARNING!  This is omega!  Program will be stuck if run in unprotected mode. :]
((\x. (x x)) (\x. (x x)));

if true then (x x) else true;
succ succ succ succ succ pred pred 100;
0;

(\x. (x x));

if (iszero 12) then 42 else 1;

((\f. (\z. f z)) f);
((\x. (y z)) (\y. (x y)));
if (iszero 0) then 2 else 4;
(\x. (x x));

