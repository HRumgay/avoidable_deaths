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
data(slopop)

#Converting to a matrix...
men3<-Thailand_popmort%>%
  filter(sex==1)%>%
  rename("year"="X_year")%>%
  rename("age"="X_age")

women3<-Thailand_popmort%>%
  filter(sex==2)%>%
  rename("year"="X_year")%>%
  rename("age"="X_age")

men<-matrix(NA, 100, 15, dimnames = list(c(seq(0,99,by=1)), c(seq(2000,2014,by=1))))

women<-matrix(NA, 100, 15, dimnames = list(c(seq(0,99,by=1)), c(seq(2000,2014,by=1))))



for(j in 2000:2014){
  for(i in 1:100){
  men2<-men3%>%
    filter(year==j)
  men[i,j-1999] <- men2[i,]$prob

    women2<-women3%>%
    filter(year==j)
  women[i,j-1999] <- women2[i,]$prob
  }
}


ratetablepop<-transrate(men,women,yearlim=c(2000,2014),int.length=1) #if even one column that is unused has NA values it fails to calculated in the loop below...

SurvExpNew <- rep(0,1000)
SurvExpNew_age_cats <- matrix(ncol = 2, nrow = 20)

Time <- seq(0,5,le=1001)[-1]

for (j in 0:20){
  for (i in 1:1000){

    DataTemp <- expand.grid(age=j*5:5*(j+1)-1,year=2009:2014)
    DataTemp$year <- as.Date(paste0(DataTemp$year,"-01-01"),origin="1960-01-01",format="%Y-%m-%d")
    DataTemp$cens <- 0 ## actually, not used in the calculations...
    DataTemp$timeFix <- 0
    DataTemp$sex <- 1
    DataTemp$w <- 1/dim(DataTemp)[1]  ## or other weights if you can find convenient values to represent the combined distribution of ages at diagnosis and year at diagnosis
    

  DataTemp$timeFix <- Time[i]
  Temp <- calcExpect(time="timeFix",event="cens", ratetable=ratetablepop, rmap=list(age=age*365.241,year=year,sex=sex),data=DataTemp)
  Temp$surv <- exp(-Temp$MUA)
  SurvExpNew[i] <- sum(Temp$surv*Temp$w)
 
  
  SurvExpNew_age_cats[j,]<-c(j,SurvExpNew[1000])
  }
}



# SurvExpNew <- rep(0,1000)
# for (i in 1:1000){
#   DataTemp$timeFix <- Time[i]
#   Temp <- calcExpect(time="timeFix",event="cens",ratetable=ratetablepop,rmap=list(age=age*365.241,year=year,sex=sex),data=DataTemp)
#   Temp$surv <- exp(-Temp$MUA)
#   SurvExpNew[i] <- sum(Temp$surv*Temp$w)
# }
# 

