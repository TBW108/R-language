data<-read.csv("/Users/tbw/Desktop/R/data/data3.3.csv")
lm3_3<-lm(y~x1+x2+x3+x4+x5,data)
library(car)
vif(lm3_3)

# 条件数
XX <- cor(data[,3:7])
kappa(XX,exact=TRUE) # 存在严重的多重共线性
eigen(XX) # 计算特征根找到线性相关系数

# 消除x1，因为其VIF最大
summary(drop1_lm3_3 <- lm(y~x2+x3+x4+x5,data = data))
vif(drop1_lm3_3) # 还可以继续剔除x2

summary(drop12_lm3_3 <- lm(y~x3+x4+x5,data = data))
vif(drop12_lm3_3) 

# 标准化
library(QuantPsyc)
lm.beta(drop12_lm3_3) #经济解释合理


