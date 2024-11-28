library(TSA)
library(aTSA)

setwd("C:/Users/jun99/OneDrive - 계명대학교/바탕 화면/stat/Time series")
data.set <- read.csv( "time.csv",  fileEncoding = "euc-kr" )
data.set_2 <- read.csv( "time_2.csv",  fileEncoding = "euc-kr" )
data2<-data.set_2[-1,]

bmi <- round(data2$BMI, 2)
kcal <- round(data2$섭취.kcal, 2)
kg <- round(data2$몸무게.kg., 2)
체지방 <- round(data2$체지방률..., 2)
골격근량 <- round(data2$골격근량.kg., 2)

par(mfrow=c(1,1))
plot(bmi, type ='l', main='bmi')
plot(체지방, type ='l',main='체지방')
plot(kg, type ='l', main='kg')
plot(골격근량, type ='l', main='골격근량')
plot(kcal, type ='l', main='kcal')

adf.test(bmi)
adf.test(bmi - mean(bmi))

adf.test(kg)
adf.test(kg - mean(kg))

adf.test(체지방)
adf.test(체지방 - mean(체지방))

adf.test(kcal - mean(kcal))

plot(diff(체지방), type='l')
#stationary인 것 같음

matplot(  y = cbind( kcal , 체지방 ) , type = "l" , ylab = "체지방률(%)" , xlab = "섭취칼로리" , lty = 1 )
legend( "topright" , legend = c( "섭취칼로리" , "체지방" ) , lty = 1 , col = 1:2 )

adf.test( 체지방 - mean( 체지방 ) , output = F )$type1
adf.test( Sale - mean( Sale ) , output = F )$type1

X <- kcal
Y <- 체지방

# 젤 작은 값을 선택하기 위해
aic <- matrix( data = Inf , nrow = 5 , ncol = 7 )
for( i in 1:4 )
{
  for( j in 1:4 )
  {
    aic[i,j] <- stats::arima( x = X , order = c(i-1,0,j-1) , method = "ML" )$aic
  }
}
aic

# 젤 작은 값을 선택하기 위해
aic <- matrix( data = Inf , nrow = 5 , ncol = 7 )
for( i in 1:4 )
{
  for( j in 1:4 )
  {
    aic[i,j] <- stats::arima( x = X , order = c(i-1,1,j-1) , method = "ML" )$aic
  }
}
min(aic)
# 행렬을 열벡터로 만들어서 결과를 출력함
which(aic==min(aic), arr.ind=TRUE)

# arr.ind, array index
order <- integer(3)
order[1] <- which( aic == min( aic ) , arr.ind = TRUE )[1,1]-1
order[3] <- which( aic == min( aic ) , arr.ind = TRUE )[1,2]-1

fit.X <- stats::arima( x = X , order = order , method = "ML" )

tsdiag( fit.X )
fit.X

# prewhitening
Z <- X
Z <- filter( x = Z  , filter = -fit.X$model$theta , method = "recursive" )
Z <- filter( x = Z  , filter = c( 1 , -fit.X$model$phi ) , method = "convolution" , sides = 1 )
U <- Y
U <- filter( x = U  , filter = -fit.X$model$theta , method = "recursive" )
U <- filter( x = U  , filter = c( 1 , -fit.X$model$phi ) , method = "convolution" , sides = 1 )

# 이걸 어떻게 해석할 것인가.
# beta(b)식별하는 과정, delay x, truncation x(정해야 할 모수 많아짐),
# decaning o(모수 정하기 좋음)
# b=0 , q=0, p=1
ccf( x = U , y = Z , na.action = na.omit , xlim = c(0,15) )

prewhiten( x = X , y = Y , x.model = fit.X , xlim=c(0,15))


# p,g
transfer.order <- list( c(1,0) )

aic <- matrix( data = Inf , nrow = 5 , ncol = 4 )
# conceptually equivalent to arimax(arimax( Y , order = c(0,0,0) , xtransf = X , transfer = transfer.order)
aic[1,1] <- arimax( Y , order = c(1,0,0) , xtransf = X , transfer = transfer.order , fixed = c(0,NA,NA,NA) )$aic
for( i in 1:5 )
{
  for( j in 1:4 )
  {
    if( i > 1 || j > 1 )
    {
      try( {
        aic[i,j] <- arimax( Y , order = c(i-1,0,j-1) , xtransf = X , transfer = transfer.order )$aic
      } )
    }
  }
}

aic
