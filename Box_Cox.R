install.packages('MASS')
library(MASS)
# 例3.2消除异方差
data3.2 <- read.csv("D:/STUDY/code/R/data/data3.2.csv")

bc3.2 <- boxcox(y ~ x1 + x2, data = data3.2, lambda = seq(-2,2,0.01))
lambda <- bc3.2$x[which.max(bc3.2$y)]
lambda

# 计算变换后的y值
y_bc <- (data3.2$y^lambda - 1) / lambda

lm3.2_bc <- lm(y_bc ~ x1 + x2, data = data3.2)
summary(lm3.2_bc)

abse<-abs(resid(lm3.2_bc))
cor.test(data3.2$x1, abse, method = "spearman")
cor.test(data3.2$x2, abse, method = "spearman")


# 例2.2消除自相关
data2.2 <- read.csv("D:/STUDY/code/R/data/data2.2.csv")
bc2.2 <- boxcox(y ~ x, data = data2.2, lambda = seq(-2, 2, 0.01))
lambda <- bc2.2$x[which.max(bc2.2$y)]
lambda

# 计算变换后的y值
y_bc <- (data2.2$y^lambda - 1) / lambda
summary(lm2.2_bc <- lm(y_bc~x, data = data2.2))

install.packages("lmtest")
install.packages("zoo")
library(lmtest)

dwtest(lm2.2_bc,alternative = "two.sided")
---