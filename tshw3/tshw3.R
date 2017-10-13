install.packages("astsa")
library("astsa")
setwd("~/Desktop/tshw3/")
data1=read.csv("data1.csv")
data2=read.csv("data2.csv")
plot.ts(data1[,2])
plot.ts(data2[,2])

diff.data1=diff(data1[,2])
plot.ts(diff.data1)
acf2(diff.data1)


diff.data2=diff(data2[,2])
plot.ts(diff.data2)
acf2(diff.data2)

#4,5
model_data1_p4=arima(data1[,2],order = c(4,1,0))
tsdiag(model_data1_p4,gof=12)
model_data1_p5=arima(data1[,2],order = c(5,1,0))
tsdiag(model_data1_p5,gof=12)
model_data2_p4=arima(data2[,2],order = c(4,1,0))
tsdiag(model_data2_p4,gof=12)
model_data2_p5=arima(data2[,2],order = c(5,1,0))
tsdiag(model_data2_p5,gof=12)
model_data2_p9=arima(data2[,2],order = c(9,1,0))
tsdiag(model_data2_p9,gof=12)

#6
result1= predict(model_data1_p5,n.ahead = 12)
result2= predict(model_data2_p9,n.ahead = 12)

result1= predict(model_data1_p5,n.ahead = 12)
plot(1:length(data1[,2]),data1[,2],type='l')
points(length(data1[,2]):(length(data1[,2])+11),result1$pred,pch='0',cex=0.2)

