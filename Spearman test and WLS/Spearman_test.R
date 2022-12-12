library(xlsx)
data4_3<-read.xlsx('/Users/tbw/Desktop/R/Applied regression analysis/data4-3.xlsx',1)
X=data4_3$x
Y=data4_3$y

# 去除里面的空格
Y= gsub('[ ]', '', Y)
Y=as.numeric(Y)

# 拟合
lm4_3<-lm(y~x,data4_3)
summary(lm4_3)
e<-resid(lm4_3)

# 作图
attach(data4_3)
plot(X,e)
abline(h=c(0),lty=5)
detach(data4_3)

# Spearman检验
abse<-abs(e)
# 检测是否有异方差
cor.test(data4_3$x,abse,alternative = "two.sided",method = "spearman",conf.level = 0.95)


s<-seq(-2,2,0.5)
result1<-vector(length = 9,mode='list')
result2<-vector(length = 9,mode='list')

for(j in 1:9)
{
  w<-data4_3$x^(-s[j])
  lm4<-lm(y~x,weights = w,data4_3)
  result1[[j]]<-logLik(lm4)
  result2[[j]]<-lm4
}
result1

ew<-resid(result2[[8]])

plot(data4_3$x,ew)
