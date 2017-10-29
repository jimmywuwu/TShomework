
library(tseries)
series1=read.table("arima1.txt")
series2=read.table("arima2.txt")

#1 arima1.txt

# no
plot.ts(series1)

# D=1
plot.ts(diff(series1$x))

# ADF test , beta < 1 in significance level 0.01
adf.test(diff(series1$x),alternative="stationary",k=20)

# 
acf2(diff(series1$x))
res1=arima(series1$x,c(2,1,3))
tsdiag(res1)

#