NAME malformed_indicator_value
OBJSENSE MAX
ROWS
 N  obj
 G  con1
COLUMNS
    x         obj       1              con1      1
    z         obj       0   
RHS
    rhs       con1      1
RANGES
BOUNDS
 FR bounds    x         0
 BV bounds    z         1
INDICATORS
 IF con1      z         2
ENDATA
