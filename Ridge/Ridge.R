data <- read.csv('/Users/tbw/Desktop/R/data/data3.3.csv')
datas <- data.frame(scale(data[,2:7])) #标准化处理
library(MASS)
ridge <- lm.ridge(y~.-1,data=datas,lambda = seq(0,3,0.1)) #lambda为参数的所有值
beta <- coef(ridge)
beta
k <- ridge$lambda
plot(k,k,type="n")
linetype <- c(1:5)
char <- c(18:22)
for (i in 1:5) {
  lines(k,beta[,i],type="o",lty=linetype[i],pch=char[i],cex=0.75)  
}
legend(locator(1),inset=0.5,legend=c("x1","x2","x3","x4","x5"),cex=0.8,pch=char,lty=linetype)
