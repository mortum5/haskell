module Cps where

decode f = f (0)
as x f = f x
a x f = f x
number x = x

one x f = f (x + 1)
two x f = f (x + 2)
three x f = f (x + 3)
seventeen x f = f (x + 17)
twenty x f = f (x + 20)
hundred x f = f (x + 100)
thousand x f= f (x + 1000)
