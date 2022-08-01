minimize
obj: [ x * x + x * y ]    \ Objective with no /2
Subject To
c1: [ x^2 - x * y ] <= 0.0
c2: [ x^2 - x * y ]/2 <= 0.0
Bounds
x free
y free
End
