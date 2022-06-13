##########################################################
#
# Expected survival for all countries, ages and sex
#
##########################################################

library("relsurv")
library("mexhaz")
library("data.table")
library("numbers")

load("popmort2.RData")
load("country_codes.RData")

# combine age groups 0 (<1 yr) and 1 (1-4 yr) in popmort2
popmort2 %>% 
  group_by(country_code,sex,year) %>% 
  mutate(nLx=case_when(age==1 ~ sum(nLx[age%in%c(0,1)]),
                       TRUE~nLx),
         ndx=case_when(age==1 ~ sum(ndx[age%in%c(0,1)]),
                       TRUE~ndx)
         ) %>%
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

men2<-p%>%
  filter(sex==1)%>%
  mutate(prob=1-ndx/nLx)

men2001<-men2%>%
  filter(year==2000)%>%
  mutate(year=2001)
men2002<-men2%>%
  filter(year==2000)%>%
  mutate(year=2002)
men2003<-men2%>%
  filter(year==2000)%>%
  mutate(year=2003)

men2004<-men2%>%
  filter(year==2000)%>%
  mutate(year=2004)

men2006<-men2%>%
  filter(year==2005)%>%
  mutate(year=2006)
men2007<-men2%>%
  filter(year==2005)%>%
  mutate(year=2007)
men2008<-men2%>%
  filter(year==2005)%>%
  mutate(year=2008)
men2009<-men2%>%
  filter(year==2005)%>%
  mutate(year=2009)

men2011<-men2%>%
  filter(year==2010)%>%
  mutate(year=2011)
men2012<-men2%>%
  filter(year==2010)%>%
  mutate(year=2012)
men2013<-men2%>%
  filter(year==2010)%>%
  mutate(year=2013)
men2014<-men2%>%
  filter(year==2010)%>%
  mutate(year=2014)


women2<-p%>%
  filter(sex==2)%>%
  mutate(prob=1-ndx/nLx)


women2001<-women2%>%
  filter(year==2000)%>%
  mutate(year=2001)
women2002<-women2%>%
  filter(year==2000)%>%
  mutate(year=2002)
women2003<-women2%>%
  filter(year==2000)%>%
  mutate(year=2003)
women2004<-women2%>%
  filter(year==2000)%>%
  mutate(year=2004)

women2006<-women2%>%
  filter(year==2005)%>%
  mutate(year=2006)
women2007<-women2%>%
  filter(year==2005)%>%
  mutate(year=2007)
women2008<-women2%>%
  filter(year==2005)%>%
  mutate(year=2008)
women2009<-women2%>%
  filter(year==2005)%>%
  mutate(year=2009)

women2011<-women2%>%
  filter(year==2010)%>%
  mutate(year=2011)
women2012<-women2%>%
  filter(year==2010)%>%
  mutate(year=2012)
women2013<-women2%>%
  filter(year==2010)%>%
  mutate(year=2013)
women2014<-women2%>%
  filter(year==2010)%>%
  mutate(year=2014)

men3<-data.table(men2%>%
                   full_join(men2001)%>%
                   full_join(men2002)%>%
                   full_join(men2003)%>%
                   full_join(men2004)%>%
                   full_join(men2006)%>%
                   full_join(men2007)%>%
                   full_join(men2008)%>%
                   full_join(men2009)%>%
                   full_join(men2011)%>%
                   full_join(men2012)%>%
                   full_join(men2013)%>%
                   full_join(men2014))

women3<-data.table(women2%>%
                     full_join(women2001)%>%
                     full_join(women2002)%>%
                     full_join(women2003)%>%
                     full_join(women2004)%>%
                     full_join(women2006)%>%
                     full_join(women2007)%>%
                     full_join(women2008)%>%
                     full_join(women2009)%>%
                     full_join(women2011)%>%
                     full_join(women2012)%>%
                     full_join(women2013)%>%
                     full_join(women2014))


ES_list <- lapply(unique(country_codes$country_code), function(k) { #Looping through the countries
  #new country_codes data has 186 countries
  
  #Aggregating life table data forward and converting to a matrix...
  men2<-men3[country_code==k,]
  women2<-women3[country_code==k,]
  
  men<-matrix(NA, 18, 15, dimnames = list(c(seq(1,18,by=1)), c(seq(2000,2014,by=1))))
  
  women<-matrix(NA, 18, 15, dimnames = list(c(seq(1,18,by=1)), c(seq(2000,2014,by=1))))
  
  
  for(j in 2000:2014){
    for(i in 1:18){
      men2<-men4[year==j,]
      men[i,j-1999] <- men2[i,]$prob
      
      women2<-women4[year==j,]
      women[i,j-1999] <- women2[i,]$prob
    }
  }
  
  
  
  ratetablepop<-transrate(men,women,yearlim=c(2000,2014),int.length=1) #if even one column that is unused has NA values it fails to calculated in the loop below...
  
  SurvExpNew_1 <- data.table(rep(0, 1000),1:1000)
  SurvExpNew_2 <- data.table(rep(0, 1000),1:1000)

  
  
  
  Time <- seq(0,5,le=1001)[-1]
  
  
  t2 <- lapply(1:18, function(j) {
    cat(toString(k), ", ", toString(j),"\n") #print country + age group
 
    DataTemp <- expand.grid(age=j,year=2014)   #
    DataTemp$year <- as.Date(paste0(DataTemp$year,"-01-01"),origin="1960-01-01",format="%Y-%m-%d")
    DataTemp$cens <- 0 # actually, not used in the calculations...
    DataTemp$timeFix <- 0
    DataTemp$sex <- 1
    DataTemp$w <- 1/dim(DataTemp)[1]  # or other weights if you can find convenient values to represent the combined distribution of ages at diagnosis and year at diagnosis
    
    
    DataTemp2 <- expand.grid(age=j,year=2014)   #
    DataTemp2$year <- as.Date(paste0(DataTemp2$year,"-01-01"),origin="1960-01-01",format="%Y-%m-%d")
    DataTemp2$cens <- 0 ## actually, not used in the calculations...
    DataTemp2$timeFix <- 0
    DataTemp2$sex <- 2
    DataTemp2$w <- 1/dim(DataTemp2)[1]  ## or other weights if you can find convenient values to represent the combined distribution of ages at diagnosis and year at diagnosis
    
    for (i in 1000){
      
      DataTemp$timeFix <- Time[i]
      Temp <- calcExpect(time="timeFix",
                         event="cens", 
                         ratetable=ratetablepop, 
                         rmap=list(age=age*365.241,
                                   year=year,
                                   sex=sex),
                         data=DataTemp)
      Temp$surv <- exp(-Temp$MUA)
      
      DataTemp2$timeFix <- Time[i]
      
      Temp2 <- calcExpect(time="timeFix",
                          event="cens", 
                          ratetable=ratetablepop, 
                          rmap=list(age=age*365.241,
                                    year=year,
                                    sex=sex),
                          data=DataTemp2)
      Temp2$surv <- exp(-Temp2$MUA)
      
      SurvExpNew_1[i,1] <- sum(Temp$surv*Temp$w)
      SurvExpNew_2[i,1] <- sum(Temp2$surv*Temp2$w)
      
      #SurvExpNew_age_cats_men[[(k-1)*j+j]]<-c(k, j,SurvExpNew_1[1000])
      #SurvExpNew_age_cats_women[[(k-1)*j+j]]<-c(k, j,SurvExpNew_2[1000])
    }
    t <- bind_rows(SurvExpNew_1[,sex:=1],
                   SurvExpNew_2[,sex:=2])
    setnames(t, c("V1","V2"),c("SurvExp","time"))
    t <- t[,country_code:=k][,age:=j]
  })
  t3 <- do.call(rbind.data.frame, t2)
})
ES_dt <- do.call(rbind.data.frame, ES_list)
# ES_dt is now data.table of 186 countries, each with 18 age groups and ES estimates for both sexes
save(ES_dt, file="ES_dt.RData")




countries<-p%>%summarize(country_code,country_label)%>%distinct()

<<<<<<< HEAD
SurvExpNew_age_cats_men <- matrix(ncol = 2, nrow =3515)
SurvExpNew_age_cats_women <- matrix(ncol = 2, nrow =3515)
=======
SurvExpNew_age_cats_men <- matrix(ncol = 2, nrow =18*186)
SurvExpNew_age_cats_women <- matrix(ncol = 2, nrow =18*186)
>>>>>>> 270b88ed5036a87c7a25e5b5370745191d823de1


SurvExpNew_age_cats_men[1, ]

  
ES_dt
# fix formula so it indexes correctly
# shouldn't be necessary now with ES_dt
<<<<<<< HEAD

2*185*19

  for(i in 1:(7030/3)){
     SurvExpNew_age_cats_men[i, ]<-c(mod(i-1,19),ES_dt[1000,i*2-1])
     SurvExpNew_age_cats_women[i, ]<-c(mod(i-1,19),ES_dt[2000,i*2-1])
=======
for (j in 1:18){
  for(i in 1:185){
    SurvExpNew_age_cats_men[((j)*185)+i, ]<-c(j,ES_list[[i]][[j+1]][["SurvExpNew"]][1000])
    SurvExpNew_age_cats_women[((j)*185)+i, ]<-c(j,ES_list[[i]][[j+1]][["SurvExpNew"]][2000])
>>>>>>> 270b88ed5036a87c7a25e5b5370745191d823de1
  }
mod(i-1,19)



SurvExpNew_age_cats_men2<-SurvExpNew_age_cats_men%>%
  as.data.frame()%>%
  rename("age"="V1")%>% #age coded in age groups of five years like globocan
  rename("ES"="V2")%>%
  mutate(sex=1)

Expected_Survival<-SurvExpNew_age_cats_women%>%
  as.data.frame()%>%
  rename("age"="V1")%>% #age coded in age groups of five years like globocan
  rename("ES"="V2")%>%
  mutate(sex=2)%>%
  full_join(SurvExpNew_age_cats_men2)

#write.csv(Thailand_expected_Survival, "~/Documents/R_Projects/Data/Thailand_expected_Survival.csv")

