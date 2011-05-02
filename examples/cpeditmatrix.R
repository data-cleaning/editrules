
# p = profit, c = cost, t = turnover
# 
E <- editmatrix(c(
        "p + c == t",
        "c - 0.6*t >= 0"
    ))

cc <- cp.editmatrix(E,x=c(p=755,c=125,t=200), weight=c(1,1,1))
cc$searchNext()





