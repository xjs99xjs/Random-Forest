## Harvard Data Science Project
## Jerry Xia
## Sixth Assignment
## 08/16/2019


library(randomForest)
library(foreign)
library(ggm)
library(ROCR)


# Read in Demographic data
DEMO <- read.xport("C:/Users/apple/Downloads/DEMO_I.XPT")

DEMO <- apply(DEMO,2, function(DEMO){
  if(sum(is.na(DEMO))>0){
    DEMO[is.na(DEMO)]<- quantile(DEMO,na.rm = T, probs = 0.5)
  }
  DEMO
})

# Read in Dietary Data
DR1TOT <- read.xport("C:/Users/apple/Downloads/DR1TOT_I.XPT")

DR2TOT <- read.xport("C:/Users/apple/Downloads/DR2TOT_I.XPT")

DR2FF <- read.xport("C:/Users/apple/Downloads/DR2IFF_I.XPT")

DR1FF <- read.xport("C:/Users/apple/Downloads/DR1IFF_I.XPT")

# Read in Mental Health data
DPQ <- read.xport("C:/Users/apple/Downloads/DPQ_I.XPT")

DPQ <- apply(DPQ,2, function(DPQ){
  if(sum(is.na(DPQ))>0){
    DPQ[is.na(DPQ)]<- quantile(DPQ,na.rm = T, probs = 0.5)
  }
  DPQ
})

DPQsum <- NULL
for (i in 1:5735){
  DPQsum[i] <- sum(DPQ[i,2:11])
}

DPQ_new <- data.frame(DPQ,DPQsum)




set.seed(123456)
hs <- nrow(DR1FF)
specimen <- sample(hs,0.1*hs)
reDR1 <- DR1FF[specimen,]


set.seed(111111)
ht <- nrow(DR2FF)
speciman <- sample(hs,0.1*ht)
reDR2 <- DR2FF[speciman,]



# Merge Dietary data into one
FOOD <- list(DR2TOT,DR1TOT,reDR2,reDR1)
Dietary <- Reduce(function(x,y) merge(x,y,all=T),FOOD)

# Merge DEMO and Dietary Data
var <- merge(DEMO,Dietary,by="SEQN")

var <- apply(var,2, function(var){
  if(sum(is.na(var))>0){
    var[is.na(var)]<- quantile(var,na.rm = T, probs = 0.5)
  }
  var
})

dat <- merge(DPQ_new,var)

dat <- dat[,-1:-11]

matrix <- cor(dat)




set.seed(101)
train <- sample(1:nrow(dat),10000)
test <- dat[-train,]

dat_rf <- randomForest(DPQsum~RIAGENDR+INDFMPIR+DRQSDIET+DR1TTFAT+AIALANGA
                       +DR1TS060+DR1TCAFF+DR1TKCAL+DR1TPROT+DR1TCARB
                       +DR1TSUGR+DR1TFIBE+FIAINTRP+DR1TALCO,data=dat,subset=train)
dat_rf

plot(dat_rf)

dat.predict <- predict(dat_rf,test)

oob.err=double(14)
test.err=double(14)

#mtry is no of Variables randomly chosen at each split
for(mtry in 1:14) {
  rf=randomForest(DPQsum ~ RIAGENDR+INDFMPIR+DRQSDIET+DR1TTFAT+AIALANGA
                  +DR1TS060+DR1TCAFF+DR1TKCAL+DR1TPROT+DR1TCARB
                  +DR1TSUGR+DR1TFIBE+FIAINTRP+DR1TALCO , data = dat , subset = train,mtry=mtry,ntree=400) 
  oob.err[mtry] = rf$mse[400] #Error of all Trees fitted
  
  pred<-predict(rf,test) #Predictions on Test Set for each Tree
  test.err[mtry]= with(dat[-train,], mean( (DPQsum - pred)^2))
  cat(mtry," ")
}


test.err
oob.err

matplot(1:mtry , cbind(oob.err,test.err), pch=19 , col=c("red","blue"),type="b",ylab="Mean Squared Error",
        xlab="Number of Predictors Considered at each Split",ylim=c(5,14))
legend("topright",legend=c("Out of Bag Error","Test Error"),pch=19, col=c("red","blue"))


importance(dat_rf)
varImpPlot(dat_rf)


