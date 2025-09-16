\ a comment to begin with
\ weirdly, V ar4 and Va r4 are the same variable...
\ we're going to disallow this and say the file is corrupt
\ its a terrible idea
Max \ this problem is a maximisation!

obj: 
-1 Var4
 + 1 V5 + [ Var4^2
 - 1.2 V5 * V1 ] /2
Subject To
CON1:
    1 V1 >= 0.0
1 V2 >= 2.0 \ not named
CON3: 1 V3 <= 2.5
CON4: 1 V5 + 1 V6   \ split constraint. we know it hasn't ended as missing operator
 + 1 V7 <= 1.0

CON5: [ Var4^2 - 1.2 V5 * V1 ] <= 0.0
Bounds
-inf <= V1 <= 3
V2 <= 3
V3 >= -3
+inf >=  Var4 >= 5.5
V5 = 1 \ fixed variable

V6 free
0 <= V7 < 1 \ stupidly allow < as <=
0 <= V8 <= 1
General
Var4 V5 \ integer variables can be listed (Mosek)
V6 \ or each new line
Binary
V8
SOS
 sos1: S1:: V1 : 1 V2 : 2 V3 : 3
 sos2: S2 :: V1:8.5 V2:10.2 V3:18.3
End
