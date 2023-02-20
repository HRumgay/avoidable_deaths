library("relsurv")
library("mexhaz")
library(readstata13)
library(data.table)
setwd("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\Breast Cancer")
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

missing_CC<-data.frame(c("Seychelles"), c(10001))
colnames(missing_CC) <- c("country_label","country_code")


country_codes <-
  read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\GCO_country_info.csv", stringsAsFactors = FALSE) %>% 
  filter(country_code<900) %>% 
  mutate(country_label = replace(country_label, country_label == "Iran, Islamic Republic of", "Iran")) %>%
  mutate(country_label = replace(country_label, country_label == "Korea, Republic of", "South Korea")) %>%
  mutate(country_label = replace(country_label, country_label == "France, Martinique", "Martinique")) %>%
  mutate(country_label = replace(country_label, country_label ==  "C?te d'Ivoire","Cote d'Ivoire")) %>%
  select(country_code, country_label)%>% 
  full_join(missing_CC)


#life tables
life_file_list<-list.files('\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\lifetables\\Expanded_2018', full.names=TRUE)
life<-plyr::ldply(life_file_list,read.dta13)
life<-life%>%as.data.frame()%>%
  clean_names()%>%
  filter(!is.na(mx))%>%
  select(-country)%>%
  filter(region!="Martinique")%>%
  filter(region!="Mauritius")


Iran<-read.dta13(life_file_list[15])%>%as.data.frame()
Iran<-Iran%>% clean_names()%>%rename("country"="region")

Martinique<-read.dta13(life_file_list[21])%>%as.data.frame()
Martinique<-Martinique%>%select(-country)%>% clean_names()#%>%rename("country"="region")

Mauritius<-read.dta13(life_file_list[22])%>%as.data.frame()
Mauritius<-Mauritius%>%clean_names()#%>%rename("region"="country")

puerto_rico<-read.dta13(life_file_list[27])%>%as.data.frame()
puerto_rico<- puerto_rico%>%clean_names()#%>%rename("region"="country")

life<-life%>%full_join(Mauritius)%>%full_join(Martinique)



#Load mortality rates for survival analysis. Correct and use probability
life_table<-life%>%
 # filter(sex==2)%>%
  select(region,year,sex,age,mx,prob)%>%
  mutate(region=replace(region,region=="Korea","South Korea"))%>%
  mutate(region=replace(region,region=="South_Africa","South Africa"))%>%
  mutate(region=replace(region,region=="Cote_D`ivoire","Cote d'Ivoire"))%>%
  mutate(region=replace(region,region=="Saudi_Arabia","Saudi Arabia"))%>%
  mutate(region=replace(region,region=="Costa_Rica","Costa Rica"))%>%
  mutate(region=replace(region,region=="Bahain","Bahrain"))%>%
  mutate(region=replace(region,region=="Costa_Rica","Costa Rica"))%>%
  mutate(region=replace(region,region=="Ethiopy","Ethiopia"))

# 
# #adding the mortality rates of 2015 to the years 2016-2020
# 
life_table_2019<-life_table%>% 
  filter(year==2015)%>%
  mutate(year=replace(year,year==2015,2019))
life_table_2020<-life_table%>% 
  filter(year==2015)%>%
  mutate(year=replace(year,year==2015,2020))

life_table_complete<-life_table %>%
  full_join(life_table_2019) %>%
  full_join(life_table_2020) %>%
  left_join(country_codes, by = c("region"="country_label"))

countries_survcan<-life_table_complete%>%
  select(region,country_code)%>%distinct()%>%as.data.frame()%>%
  filter(!is.na(country_code))%>%as.data.table()
expected_survcan<-list()

# for(p in(1:nrow(countries_survcan))){
# 
SURVCAN_popmort<-life_table_complete%>%
   dplyr::rename("country"="region")%>%
  dplyr::mutate(
    age_cat = case_when(
      age>=15 & age<65 ~ "15-64",
      age>= 65~ "65-99",
      age<15 ~"0-15"
      ))%>%
    ungroup()%>%
  # select(-age)%>%
  # dplyr::rename("age"="age_cat")%>%
  filter(age!=0)%>%
  as.data.table()


# #Converting to a matrix...
men3<-SURVCAN_popmort%>%
  filter(!is.na(country_code))%>%
  filter(sex==1)%>%
  as.data.table()

women3<-SURVCAN_popmort%>%
  filter(!is.na(country_code))%>%
  filter(sex==2)%>%
  as.data.table()

country_codes_survcan<-SURVCAN_popmort%>%
  select(country_code)%>%
  distinct()%>%
  filter(!is.na(country_code))

ES_list <- lapply(unique(country_codes_survcan$country_code), function(k) {
  #Looping through the countries
  
  #Aggregating life table data forward and converting to a matrix...
  men4<-men3[country_code==k,]
  
  women4<-women3[country_code==k,]
  
  men <-
    matrix(NA, 100, 15, dimnames = list(c(seq(1,100, by = 1)), c(seq(2000, 2014, by =
                                                                   1))))
  
  women <-
    matrix(NA, 100, 15, dimnames = list(c(seq(1,100, by = 1)), c(seq(2000, 2014, by =
                                                                     1))))
  
  
  for(j in 2000:2014){ 
    for(i in 1:100){
      men2<-men4[year==j,]
      men[i,j-1999] <- men2[i,]$prob
      
      women2<-women4[year==j,]
      women[i,j-1999] <- women2[i,]$prob
    }
  }
  
  ratetablepop <-
    transrate(men,
              women,
              yearlim = c(2000, 2014),
              int.length = 1) #if even one column that is unused has NA values it fails to calculated in the loop below...
  
  SurvExpNew_1 <- data.table(rep(0, 1000),1:1000)
  SurvExpNew_2 <- data.table(rep(0, 1000),1:1000)
  #SurvExpNew_age_cats_men <- matrix(ncol = 2, nrow = 20)
  #SurvExpNew_age_cats_women <- matrix(ncol = 2, nrow = 20)
  
  
  Time <- seq(0, 5, le = 1001)[-1]
  
  t2 <- lapply(1:2, function(j) { 
    cat(toString(k), ", ", toString(j),"\n")
    #Update age here. We have groups only
    DataTemp <- expand.grid(age=j,year=2014)   
    DataTemp$year <- as.Date(paste0(DataTemp$year,"-01-01"),origin="1960-01-01",format="%Y-%m-%d")
    DataTemp$cens <- 0 ## actually, not used in the calculations...
    DataTemp$timeFix <- 0
    DataTemp$sex <- 1
    DataTemp$w <- 1/dim(DataTemp)[1]  ## or other weights if you can find convenient values to represent the combined distribution of ages at diagnosis and year at diagnosis
    
    
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
                         rmap=list(age=age*365.241, #to change?
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
      
      # SurvExpNew_age_cats_men[k,]<-c(j,SurvExpNew_1[1000])
      # SurvExpNew_age_cats_women[k,]<-c(j,SurvExpNew_2[1000])
    }
    t <- bind_rows(SurvExpNew_1[,sex:=1],
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
ES_list <- lapply(unique(country_codes_survcan$country_code), function(k) {
  #Looping through the countries
  
  #Aggregating life table data forward and converting to a matrix...
  men4<-men3[country_code==k,]
  
  women4<-women3[country_code==k,]
  
  men <-
    matrix(NA, 2, 15, dimnames = list(c(seq(1,2, by = 1)), c(seq(2000, 2014, by =
                                                                     1))))
  
  women <-
    matrix(NA, 2, 15, dimnames = list(c(seq(1,2, by = 1)), c(seq(2000, 2014, by =
                                                                     1))))
  
  
  for(j in 2000:2014){ 
    for(i in 1:2){
      men2<-men4[year==j,]
      men[i,j-1999] <- men2[i,]$prob
      
      women2<-women4[year==j,]
      women[i,j-1999] <- women2[i,]$prob
    }
  }
  
  ratetablepop <-
    transrate(men,
              women,
              yearlim = c(2000, 2014),
              int.length = 1) #if even one column that is unused has NA values it fails to calculated in the loop below...
  
  SurvExpNew_1 <- data.table(rep(0, 1000),1:1000)
  SurvExpNew_2 <- data.table(rep(0, 1000),1:1000)
  #SurvExpNew_age_cats_men <- matrix(ncol = 2, nrow = 20)
  #SurvExpNew_age_cats_women <- matrix(ncol = 2, nrow = 20)
  
  
  Time <- seq(0, 5, le = 1001)[-1]
  
  t2 <- lapply(1:2, function(j) { 
    cat(toString(k), ", ", toString(j),"\n")
    #Update age here. We have groups only
    DataTemp <- expand.grid(age=j,year=2014)   
    DataTemp$year <- as.Date(paste0(DataTemp$year,"-01-01"),origin="1960-01-01",format="%Y-%m-%d")
    DataTemp$cens <- 0 ## actually, not used in the calculations...
    DataTemp$timeFix <- 0
    DataTemp$sex <- 1
    DataTemp$w <- 1/dim(DataTemp)[1]  ## or other weights if you can find convenient values to represent the combined distribution of ages at diagnosis and year at diagnosis
    
    
    DataTemp2 <- expand.grid(age=j,year=2014)   #
    DataTemp2$year <- as.Date(paste0(DataTemp2$year,"-01-01"),origin="1960-01-01",format="%Y-%m-%d")
    DataTemp2$cens <- 0 ## actually, not used in the calculations...
    DataTemp2$timeFix <- 0
    DataTemp2$sex <- 2
    DataTemp2$w <- 1/dim(DataTemp2)[1]  ## or other weights if you can find convenient values to represent the combined distribution of ages at diagnosis and year at diagnosis
    
    for (i in 1:1000){
      
      DataTemp$timeFix <- Time[i]
      Temp <- calcExpect(time="timeFix",
                         event="cens", 
                         ratetable=ratetablepop, 
                         rmap=list(age=age*365.241, #to change?
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
      
      # SurvExpNew_age_cats_men[k,]<-c(j,SurvExpNew_1[1000])
      # SurvExpNew_age_cats_women[k,]<-c(j,SurvExpNew_2[1000])
    }
    t <- bind_rows(SurvExpNew_1[,sex:=1],
                   SurvExpNew_2[,sex:=2])
    setnames(t, c("V1","V2"),c("SurvExp","time"))
    t <- t[,country_code:=k][,age:=j]
  })
  t3 <- do.call(rbind.data.frame, t2)
  
})

ES_dt_all <- do.call(rbind.data.frame, ES_list)

dfds<-ES_dt_all%>%filter(time==1000)
save(dfds, file="ES_dt_all_survcan.RData")

#Really can't get the code to work here so tryin altgggggggg
library(survival)
# Estimate of expected  survival stratified by prior surgery 
fit1 <- survexp( ~ 1, rmap=list(sex=sex, year=year,   
                                      age=age_cat), method='conditional', data=men3)
summary(fit1, times=1:10*182.5, scale=365) #expected survival by 1/2 years


