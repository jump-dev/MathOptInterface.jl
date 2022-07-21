NAME malformed_indicator
OBJSENSE MAX
ROWS
 N  obj
 G  con1
COLUMNS
    x         obj       1              con1      1
RHS
    rhs       con1      1
RANGES
BOUNDS
 FR bounds    x         0
 BV bounds    z         1
INDICATORS
 IF con1     z
ENDATA
