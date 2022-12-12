data3.2<-read.csv('/Users/tbw/Desktop/R/data3.2.csv',header = TRUE)
lm3.2<-lm(y~x1+x2,data=data3.2)

# Spearman验证查看哪个变量的等级系数最大，选取最大的作为加权系数的分母
e<-resid(lm3.2)
abse<-abs(e)
cor.test(data3.2$x1,abse,method="spearman")
cor.test(data3.2$x2,abse,method="spearman")

# 找到最合适的参数m
s<-seq(1,5,0.5)
result1<-vector(length = 9,mode='list')
result2<-vector(length = 9,mode='list')

for(j in 1:9)
{
  w<-data3.2$x2^(-s[j])
  lm4<-lm(y~x1+x2,weights = w, data = data3.2)
  result1[[j]]<-logLik(lm4)
  result2[[j]]<-lm4
}
result1
result2[4]
summary(result2[[4]]) # 加权最小二乘法
