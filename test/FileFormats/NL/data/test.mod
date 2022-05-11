reset;
# continuous both
var x0 >= 0;
# discrete both
var x1 integer;
# continous constraint
var x2 <= 3.3, >= 2.2;
# discrete constraint
var x3 integer;
# continuous objective
var x4 == 4.4;
# discrete objective
var x5 integer;
# continous
var x6 <= 1.1;
# binary
var x7 binary;
# integer
var x8 integer;

minimize obj: 
	1 * x0 + 
	2 * x1 + 
	3 * x2 + 
	4 * x3 + 
	5 * x4 + 
	6 * x5 + 
	7 * x6 + 
	8 * x7 + 
	9 * x8 +
	sin(x0 + x1) +
	sin(x4 + x5);
   
subject to nlcon: sin(x0 + x1) == sin(x2 + x3);

option presolve 0;
write "gordering_test";

reset;
# continuous both
var x0 >= 0;
# discrete both
var x1 integer;
# continous constraint
var x2 <= 3.3, >= 2.2;
var x20 <= 3.3, >= 2.2;
var x21 <= 3.4, >= 2.3;
var x22 <= 3.5, >= 2.4;
var x23 <= 3.6, >= 2.5;
# discrete constraint
var x3 integer;
# continuous objective
var x4 == 4.4;
# discrete objective
var x5 integer;
# continous
var x6 <= 1.1;
# binary
var x7 binary;
# integer
var x8 integer;

minimize obj: 
	sin(x0 + x1) +
	sin(x4 + x5);
   
subject to nlcon: sin(x0 + x1) == sin(x2 + x3) + sin(x20 + x21 + x23 + x22);

option presolve 0;
write "gordering_test2";
