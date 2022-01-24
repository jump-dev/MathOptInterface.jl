\ File: lo1.lp 
maximize 
obj: 3 1x1 + x2 + 5 x3 + x4 
subject to 
c1:  3 x1 + x2 + 2 x3 = 30 
c2:  2 x1 + x2 + 3 x3 + x4 >= 15 
c3:  2 x2 + 3 x4 <= 25 
bounds 
 0 <= x1 <= +infinity 
 0 <= x2 <= 10 
 0 <= x3 <= +infinity 
 0 <= x4 <= +infinity 
end 