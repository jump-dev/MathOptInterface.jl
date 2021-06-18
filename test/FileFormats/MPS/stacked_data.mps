* max   x + y
* s.t.  1 <=  x <= 5 con1 G  == 1 + (-4)
*       2 <=  x <= 6 con2 L  == 6 + (+4)
*       3 <=  x <= 7 con3 E  == 3 + (4)
*       4 <= 2x <= 8 con4 E  == 8 + (-4)
*       y ∈ {1, 2, 3, 4}
*2345678901234567890123456789012345678901234567890
NAME stacked_data
OBJSENSE MAX
ROWS
 N  obj
 N  blank_obj
 G  con1
 L  con2
 E  con3
 E  con4
COLUMNS
    x         obj       1              con1      1
    x         con2      1              con3      1
    x         con4      1
    x         con4      1
    y         obj       1
    z         obj       1
    x         blank_obj 1
    y         blank_obj 1              blank_obj 1
RHS
    rhs       con1      1              con2      6
    rhs       con3      3              con4      8
RANGES
    ranges    con1      -4             con2      4
    ranges    con3      4
    ranges    con4      -4
BOUNDS
 FR bounds    x         0
 UI bounds    y         4
 LI bounds    y         1
 BV bounds    z         1
ENDATA
