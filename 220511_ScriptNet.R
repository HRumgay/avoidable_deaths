library("relsurv")
library("mexhaz")

## Function used to calculate the expected rate / cumulative rate
calcExpect <- function(time,event,id=NULL,data,ratetable,rmap,conv.time=365.241,names=c("mua","MUA")){
    if (is.null(id)){
        data$id <- 1:(dim(data)[1])
        formula <- as.formula(paste0("Surv(time=",time,"*",conv.time,",event=",event,")~id"))
    }
    else {
        formula <- as.formula(paste0("Surv(time=",time,"*",conv.time,",event=",event,")~",id))
    }
    tisd <- cbind(pmax(data[,time]*conv.time-1,0),pmax(data[,time]*conv.time,1),data[,time]*conv.time)
    if (!missing(rmap)){
        rmap <- substitute(rmap)
    }
    na.action <- NA
    rform <- relsurv:::rformulate(formula,data,ratetable,na.action,rmap)
    le <- dim(rform$data)[1]
    out <- matrix(NA,le,2)
    for (inx in 1:le) {
        temp <- as.vector(relsurv:::exp.prep(rform$R[inx,,drop=FALSE],rform$Y[inx],
            rform$ratetable,rform$status[inx],times=tisd[inx,],
            fast=FALSE)$sis)
        out[inx,] <- c((log(temp[1])-log(temp[2]))*365.241,-log(temp[3]))
    }
    out <- as.data.frame(out)
    names(out) <- names
    return(cbind(data,out))
}


data(slopop)
data(rdata)

head(rdata)

## Create time variable in years
rdata$timeY <- rdata$time/365.241

## Convert the date of diagnosis to the "%d-%m-%Y" date format
rdata$year_dg <- as.Date(rdata$year,origin="1960-01-01",format="%d-%m-%Y")

## Sex 0/1
rdata$sex01 <- 2-rdata$sex

## Dataset
MyData <- rdata ## You can change the dataset here, e.g.: subset(rdata, sex01==0)

## Create the columns "mua" corresponding to the expected hazard at
## the end of follow-up (used by mexhaz -> corresponds to the
## "expected" argument)
MyData2 <- calcExpect(time="timeY",event="cens",ratetable=slopop,rmap=list(age=age*365.241,year=year_dg,sex=sex),data=MyData)

## Model for the overall survival
ModTot <- mexhaz(Surv(time=timeY,event=cens)~1,data=MyData2,base="exp.ns",knots=quantile(MyData2[MyData2$cens==1,]$timeY,probs=c(1:2/3)))

## Model for the excess hazard
ModExc <- mexhaz(Surv(time=timeY,event=cens)~1,data=MyData2,expected="mua",base="exp.ns",knots=quantile(MyData2[MyData2$cens==1,]$timeY,probs=c(1:2/3)))

## Define the time points at which survival is evaluated
Time <- seq(0,5,le=1001)[-1]

## Predictions for each model
PredTot <- predict(ModTot,time.pts=Time)
PredExc <- predict(ModExc,time.pts=Time)

## Graph
plot(PredTot,which="surv",ylim=c(0,1),col="blue")
lines(PredExc,which="surv",col="red")

## Compute the expected survival for the study population
## (the warnings() can be discarded in that case...)
## If you just want the expected survival at 5 years, you just need to
## perform the calculation once with timeFix=5
MyData$timeFix <- 0
SurvExp <- rep(0,1000)
for (i in 1:1000){
    MyData$timeFix <- Time[i]
    Temp <- calcExpect(time="timeFix",event="cens",ratetable=slopop,rmap=list(age=age*365.241,year=year_dg,sex=sex),data=MyData)
    Temp$surv <- exp(-Temp$MUA)
    SurvExp[i] <- mean(Temp$surv)
}

## It can be verified that NS*ES is very close to the overall survival
lines(Time,SurvExp*PredExc$results$surv,col="green")
