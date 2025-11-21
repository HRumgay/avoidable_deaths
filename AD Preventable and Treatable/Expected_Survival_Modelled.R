##########################################################
#
# Expected survival for all countries, ages and sex
#
##########################################################

library("relsurv")
library("mexhaz")
library("data.table")
library("numbers")


load("p.RData") # load who_ghe_group mortality file
load("country_codes.RData")


p<-life_table_complete%>% 
  left_join(country_codes, by=c("region"="country_label")) 
# combine age groups 0 (<1 yr) and 1 (1-4 yr) in lifetables file
p<-p %>% 
  group_by(country_code,year) %>% 
  # mutate(nLx=case_when(age==1 ~ sum(nLx[age%in%c(0,1)]),
  #                      TRUE~nLx),
  #        ndx=case_when(age==1 ~ sum(ndx[age%in%c(0,1)]),
  #                      TRUE~ndx)
  #       ) %>%
  filter(age!=0) -> p


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
    out[inx,] <- c((log(temp[1])-log(temp[2]))*365.241, -log(temp[3]))
  }
  out <- as.data.frame(out)
  names(out) <- names
  return(cbind(data,out))
}


data(slopop)
data(rdata)
data(slopop)


women2<-p



women3<-data.table(women2)


ES_list <- lapply(unique(country_codes$country_code), function(k) {
  #Looping through the countries
  
  #Aggregating life table data forward and converting to a matrix...
  men4<-men3[country_code==k,]
  
  women4<-women3[country_code==k,]
  
  men <-
    matrix(NA, 99, 15, dimnames = list(c(seq(1,99, by = 1)), c(seq(2000, 2014, by =
                                                                     1))))
  
  women <-
    matrix(NA, 99, 15, dimnames = list(c(seq(1,99, by = 1)), c(seq(2000, 2014, by =
                                                                     1))))
  
  
  for(j in 2000:2014){ 
    for(i in 1:99){
      # men2<-men4[year==j,]
      # men[i,j-1999] <- men2[i,]$prob
      
      women2<-women4[year==j,]
      women[i,j-1999] <- women2[i,]$prob
    }
  }
  
  ratetablepop <-
    transrate(#men,
              women,
              yearlim = c(2000, 2014),
              int.length = 1) #if even one column that is unused has NA values it fails to calculated in the loop below...
  
 # SurvExpNew_1 <- data.table(rep(0, 1000),1:1000)
  SurvExpNew_2 <- data.table(rep(0, 1000),1:1000)
  #SurvExpNew_age_cats_men <- matrix(ncol = 2, nrow = 20)
  #SurvExpNew_age_cats_women <- matrix(ncol = 2, nrow = 20)
  
  
  Time <- seq(0, 5, le = 1001)[-1]
  
  t2 <- lapply(1:99, function(j) { 
    cat(toString(k), ", ", toString(j),"\n")
    #Update age here. We have groups only
    # DataTemp <- expand.grid(age=j,year=2014)   
    # DataTemp$year <- as.Date(paste0(DataTemp$year,"-01-01"),origin="1960-01-01",format="%Y-%m-%d")
    # DataTemp$cens <- 0 ## actually, not used in the calculations...
    # DataTemp$timeFix <- 0
    # DataTemp$sex <- 1
    # DataTemp$w <- 1/dim(DataTemp)[1]  ## or other weights if you can find convenient values to represent the combined distribution of ages at diagnosis and year at diagnosis
    # 
    
    DataTemp2 <- expand.grid(age=j,year=2014)   #
    DataTemp2$year <- as.Date(paste0(DataTemp2$year,"-01-01"),origin="1960-01-01",format="%Y-%m-%d")
    DataTemp2$cens <- 0 ## actually, not used in the calculations...
    DataTemp2$timeFix <- 0
    DataTemp2$sex <- 2
    DataTemp2$w <- 1/dim(DataTemp2)[1]  ## or other weights if you can find convenient values to represent the combined distribution of ages at diagnosis and year at diagnosis
    
    for (i in 1000){
      
      # DataTemp$timeFix <- Time[i]
      # Temp <- calcExpect(time="timeFix",
      #                    event="cens", 
      #                    ratetable=ratetablepop, 
      #                    rmap=list(age=age*365.241, #to change?
      #                              year=year,
      #                              sex=sex),
      #                    data=DataTemp)
      # Temp$surv <- exp(-Temp$MUA)
      # 
      DataTemp2$timeFix <- Time[i]
      
      Temp2 <- calcExpect(time="timeFix",
                          event="cens", 
                          ratetable=ratetablepop, 
                          rmap=list(age=age*365.241,
                                    year=year,
                                    sex=sex),
                          data=DataTemp2)
      Temp2$surv <- exp(-Temp2$MUA)
      
     #SurvExpNew_1[i,1] <- sum(Temp$surv*Temp$w)
      SurvExpNew_2[i,1] <- sum(Temp2$surv*Temp2$w)
      
      # SurvExpNew_age_cats_men[k,]<-c(j,SurvExpNew_1[1000])
      # SurvExpNew_age_cats_women[k,]<-c(j,SurvExpNew_2[1000])
    }
    t <- bind_rows(#SurvExpNew_1[,sex:=1],
                   SurvExpNew_2[,sex:=2])
    setnames(t, c("V1","V2"),c("SurvExp","time"))
    t <- t[,country_code:=k][,age:=j]
  })
  t3 <- do.call(rbind.data.frame, t2)
  
})
ES_dt <- do.call(rbind.data.frame, ES_list)
# ES_dt is now data.table of 186 countries, each with 19 age groups and ES estimates for both sexes
save(ES_dt, file="ES_dt.RData")


# all 1000 time points
ES_list <- lapply(unique(country_codes$country_code), function(k) {
  #Looping through the countries
  
  #Aggregating life table data forward and converting to a matrix...
 # men4<-men3[country_code==k,]
  
  women4<-women3[country_code==k,]
  
  # men <-
  #   matrix(NA, 18, 15, dimnames = list(c(seq(1,18, by = 1)), c(seq(2000, 2014, by =
  #                                                                    1))))
  
  women <-
    matrix(NA, 99, 15, dimnames = list(c(seq(1,99, by = 1)), c(seq(2000, 2014, by =
                                                                     1))))
  
  
  for(j in 2000:2014){ 
    for(i in 1:99){
      # men2<-men4[year==j,]
      # men[i,j-1999] <- men2[i,]$prob
      # 
      women2<-women4[year==j,]
      women[i,j-1999] <- women2[i,]$prob
    }
  }
  
  ratetablepop <-
    transrate(#men,
              women,
              yearlim = c(2000, 2014),
              int.length = 1) #if even one column that is unused has NA values it fails to calculated in the loop below...
  
 # SurvExpNew_1 <- data.table(rep(0, 1000),1:1000)
  SurvExpNew_2 <- data.table(rep(0, 1000),1:1000)
  #SurvExpNew_age_cats_men <- matrix(ncol = 2, nrow = 20)
  #SurvExpNew_age_cats_women <- matrix(ncol = 2, nrow = 20)
  
  
  Time <- seq(0, 5, le = 1001)[-1]
  
  t2 <- lapply(1:99, function(j) { 
    cat(toString(k), ", ", toString(j),"\n")
    #Update age here. We have groups only
    # DataTemp <- expand.grid(age=j,year=2014)   
    # DataTemp$year <- as.Date(paste0(DataTemp$year,"-01-01"),origin="1960-01-01",format="%Y-%m-%d")
    # DataTemp$cens <- 0 ## actually, not used in the calculations...
    # DataTemp$timeFix <- 0
    # DataTemp$sex <- 1
    # DataTemp$w <- 1/dim(DataTemp)[1]  ## or other weights if you can find convenient values to represent the combined distribution of ages at diagnosis and year at diagnosis
    # 
    
    DataTemp2 <- expand.grid(age=j,year=2014)   #
    DataTemp2$year <- as.Date(paste0(DataTemp2$year,"-01-01"),origin="1960-01-01",format="%Y-%m-%d")
    DataTemp2$cens <- 0 ## actually, not used in the calculations...
    DataTemp2$timeFix <- 0
    DataTemp2$sex <- 2
    DataTemp2$w <- 1/dim(DataTemp2)[1]  ## or other weights if you can find convenient values to represent the combined distribution of ages at diagnosis and year at diagnosis
    
    for (i in 1:1000){
      # 
      # DataTemp$timeFix <- Time[i]
      # Temp <- calcExpect(time="timeFix",
      #                    event="cens", 
      #                    ratetable=ratetablepop, 
      #                    rmap=list(age=age*365.241, #to change?
      #                              year=year,
      #                              sex=sex),
      #                    data=DataTemp)
      # Temp$surv <- exp(-Temp$MUA)
      
      DataTemp2$timeFix <- Time[i]
      
      Temp2 <- calcExpect(time="timeFix",
                          event="cens", 
                          ratetable=ratetablepop, 
                          rmap=list(age=age*365.241,
                                    year=year,
                                    sex=sex),
                          data=DataTemp2)
      Temp2$surv <- exp(-Temp2$MUA)
      
      #SurvExpNew_1[i,1] <- sum(Temp$surv*Temp$w)
      SurvExpNew_2[i,1] <- sum(Temp2$surv*Temp2$w)
      
      # SurvExpNew_age_cats_men[k,]<-c(j,SurvExpNew_1[1000])
      # SurvExpNew_age_cats_women[k,]<-c(j,SurvExpNew_2[1000])
    }
    t <- bind_rows(#SurvExpNew_1[,sex:=1],
                   SurvExpNew_2[,sex:=2])
    setnames(t, c("V1","V2"),c("SurvExp","time"))
    t <- t[,country_code:=k][,age:=j]
  })
  t3 <- do.call(rbind.data.frame, t2)
  
})

ES_dt_all <- do.call(rbind.data.frame, ES_list)
save(ES_dt_all, file="ES_dt_all.RData")
