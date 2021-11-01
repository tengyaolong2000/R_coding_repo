pnorm(1,0,1)
sum(dbinom(0:1,3, 0.2))
sum(dbinom(40:45,50,0.8))
dbinom(45, 50, 0.8)
pnorm(10,7.5,6.1) - pnorm(5, 7.5,6.1)
sum(pnorm(5:10,7.5,6.1))
qnorm(0.9, 7.5,6.1)

f<- function(x){2/(x^3)}
integrate(f,1,Inf)
fx <- function(x){(2/(x^2))}
integrate(fx,1,Inf)
?sum
install.packages("swirl")
library("swirl")
ls()
rm(ls())
rm(list=ls())
?ls
swirl()


?str
df <- read.csv('/Users/tengyaolong/Desktop/AY21_Tututorial questions/Q.csv')
df
pbinom(1,3,0.7)
dbinom(1,3,0.7)
punif(4,1,5)
dunif(4,1,5)
punif(2:3,1,5)
prod(5:6)
rnorm(5,0, 1)
choose(3,2)*prod(1:2)
dunif(4:5,1,5)
rbinom(2,6,0.5)
dbinom(0,3,0.7)
dbinom(0:3,3,0.7)
dbinom(0:1, 3,0.2)
pnorm(10,7.5,6.1) - pnorm(5,7.5,6.1)
car = read.csv(file.choose(),header=TRUE)
car$probabilities = car$frequencies/100
View(car)
car$probabilities[car$y==2]
g <-  function(x){x()}
integrate(g,0,1)
pnorm(7,0,1)
1-pnorm(7,0,1)

w5 <- read.csv(file.choose(), header=T)
View(w5)
sum(w5$probabilities[w5$x==2])
sum(w5$probabilities[w5$x>=3])
sum(w5$probabilities[w5$x<=2 & w5$y<=2])
sum(w5$probabilities[w5$x==w5$y])
sum(w5$probabilities[w5$x>w5$y])

q2 <- read.csv(file.choose(), header = T)
q2$probabilities = q2$frequencies/200
sum(q2$probabilities[q2$x==4 & q2$y==0])
View(q2)

q2$probabilities[q2$x==4]
sum(q2$frequencies[q2$x==4])
q2$frequencies[q2$x==4]/42
sum(seq(0,3)*q2$frequencies[q2$x==4]/42)
cov(q2)
cor(q2)
plot(x,y)

df = read.csv(file.choose(), header=T)
length(x)
length(y)
plot(q2$x,q2$y)


View(df)
cor(df$meantemperaturec, df$volume)
df$range = df$maximumtemperaturec - df$minimumtemperature
var(df$range)
cov(df)
?trees
d = trees
View(d)
d$price = 100*d$Height + 0.5*d$Girth
cov(d)
integrate(function(x){2*x^2},0,1)

sum(seq(1:6)*unif(1:6,1,6))
dunif(1:6,1,7)
1/6
?dunif
pUniBin(1:6,1,6)

integrate(function(x){2/(x^3)},1,Inf)

1/50*c(10,32,5,2,1)
x = c(-2,0,1,3,4)
fx = rep(0.2, 5)

c(5,4,3,2,1)-1
c(5,4,3,2,1)**2


pnorm(20.16666,21.5,(4.9323003/sqrt(30)))

g = ChickWeight
g = data("trees")
g = data("ChickWeight")

g = trees
g = ChickWeight
hist(g$weight)
summary(g$weight)
range(g$weight)
g[,c("weight","Time")]
g[c("weight","Time")]
g[ ,"weight"]
h = g[c("weight","Time")]
h = g[ ,"weight"]
library(datasets)
library("datasets")
?library
?ChickWeight
boxplot(g$weight~g$Diet)
boxplot(g$weight~g$Diet==2)

g = mtcars
View(g)
boxplot(g$mpg~g$cyl)
sum(w5$probabilities[w5$y>=3])

pnorm(80,75,5)-p
q2norm(70,75,5)

sum(car$probabilities[car$x==1])
sum(w5$probabilities[w5$x==2])
sum(q2$frequencies[ q2$x==4])
q2

cor(ChickWeight[, c('weight', 'Time')])
ChickWeight[, c('weight', 'Time')]
View(ChickWeight)
two_col = ChickWeight[, c('weight', 'Time')]

boxplot(chick$weight ~ chick$Diet)
boxplot(ChickWeight$weight ~ ChickWeight$Diet)

ques_2 = c(50, 64, 65, 80, 49, 63, 57)
min(ques_2)
max(ques_2)
mean(ques_2)
median(ques_2)
sd(ques_2)
?sd
IQR(ques_2)
ques_2
summary(ques_2)
x = rep(0,800)
for(i in 1:800){
  x[i] = mean(rnorm(3,200,5))
}
hist(x)
x
range(1:100)
choose(400, 38)/choose(400,40)
choose(40,2)/ choose(400, 2)
choose(400,38)/choose(400,2)
choose(398, 38)/ choose(400, 40)

set.seed(300)
x = runif(6,10,20)
x

a = 0
for(i in 1:100){
  a = a + i^2
}
a
rbinom(3, 10, 0.3)


x_3 = rep(0,1000)
for (i in 1:1000){
  set.seed(i)
  x_3[i] = mean(rbinom(3, 10, 0.3))
}
hist(x_3)
hist(x_40)
read.csv(file.choose(), header=T)
dbinom(3,10,0.5)
rep(1/6,6)


x = read.csv(file.choose(), header=T)
x$probabilities[x$x==2]
0.3/0.75
-0.5 * 2 *34*4 + 9*9 +2(2)(-3)*cov

r


set.seed(456)
steve = rnorm(3000,13,1)
mike = rnorm(3000,12.95
t.test(steve,mike)


fake_data = read.csv(file.choose(), header =T)
fake_data$year.factor = factor(fake_data$year)



reg_cat = lm(spending~female+year, data=fake_data)
summary(reg_cat)

x = read.csv(file.choose(), header=T
             )

REG.RESULTS = Lrv=read.csv("https://entuedumy.sharepoint.com/:x:/g/personal/cmchen_staff_main_ntu_edu_sg/EausuYX84XpFtuOFDgy5XIBNfDZElKq_Fm4saYa-R8xlA?download=1")





library(e1071)
install.packages("e1071")


x = 1
x[2] = 2

set.seed(200)
total_policy=0
total_voucher=0
for (i in 1:100){
  voucher_value=0
  policy_value=0
  contribution=rep(1000,5)
  return = runif(5,-0.03,0.07)
  for (j in 1:5){
    
    if (return[j]>0){
      policy_value[j+1]= (contribution[j]+policy_value[j])
      voucher_value[j+1]=(contribution[j]+policy_value[j])*return[j]+voucher_value[j]
    }
    else{
      policy_value[j+1]=(contribution[j]+policy_value[j])*(1+return[j])
      voucher_value[j+1]=voucher_value[j]
    }
    
  }
  total_policyvalue[i]= policy_value[6]
  total_voucher[i]= voucher_value [6] 
}
mean(total_policyvalue)
mean(total_voucher)

set.seed(200)  
stockout=0  
for (i in 1:100){  
x=rnorm(10,4.5,1.5) 
if(sum(x)>=50){  
stockout=stockout+1
}  
}
t
