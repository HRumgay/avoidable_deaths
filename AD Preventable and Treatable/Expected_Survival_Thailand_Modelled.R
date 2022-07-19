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

missing_CC<-data.frame(c("Seychelles"), c(10001))
colnames(missing_CC) <- c("country_label","country_code")


country_codes <-
  read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\GCO_country_info.csv", stringsAsFactors = FALSE) %>% 
  filter(country_code<900) %>% 
  mutate(country_label = replace(country_label, country_label == "Iran, Islamic Republic of", "Iran")) %>%
  mutate(country_label = replace(country_label, country_label == "Korea, Republic of", "South Korea")) %>%
  mutate(country_label = replace(country_label, country_label == "France, Martinique", "Martinique")) %>%
  select(country_code, country_label)%>% 
  full_join(missing_CC)

#mutate(region = replace(cancer_label, cancer_label == "Colon", "Colorectal")) %>%


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
Martinique<-Martinique%>%select(-country)%>% clean_names()%>%rename("country"="region")

Mauritius<-read.dta13(life_file_list[22])%>%as.data.frame()
Mauritius<-Mauritius%>%clean_names()%>%rename("region"="country")

puerto_rico<-read.dta13(life_file_list[27])%>%as.data.frame()
puerto_rico<- puerto_ricos%>%clean_names()%>%rename("region"="country")

life<-life%>%full_join(Mauritius)%>%full_join(Martinique)

#Load mortality rates for survival analysis. Correct and use probability
life_table<-life%>%
  #filter(sex==2)%>%
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
  full_join(life_table_2020)


Seychelles_popmort<-life_table_complete%>%
  left_join(country_codes, by = c("region"="country_label"))%>%
  dplyr::rename("country"="region")%>%
  select(-country)%>%
  filter(country_code==10001)



#Converting to a matrix...
men3<-Seychelles_popmort%>%
  filter(sex==1)

women3<-Seychelles_popmort%>%
  filter(sex==2)

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

SurvExpNew_1 <- rep(0,1000)
SurvExpNew_2 <- rep(0,1000)
SurvExpNew_age_cats_men <- matrix(ncol = 2, nrow = 20)
SurvExpNew_age_cats_women <- matrix(ncol = 2, nrow = 20)

Time <- seq(0,5,le=1001)[-1]


for (j in 0:20){
  DataTemp <- expand.grid(age=((j*5):(5*(j+1)-1)),year=2009:2014)   #
  DataTemp$year <- as.Date(paste0(DataTemp$year,"-01-01"),origin="1960-01-01",format="%Y-%m-%d")
  DataTemp$cens <- 0 ## actually, not used in the calculations...
  DataTemp$timeFix <- 0
  DataTemp$sex <- 1
  DataTemp$w <- 1/dim(DataTemp)[1]  ## or other weights if you can find convenient values to represent the combined distribution of ages at diagnosis and year at diagnosis

  
  DataTemp2 <- expand.grid(age=((j*5):(5*(j+1)-1)),year=2009:2014)   #
  DataTemp2$year <- as.Date(paste0(DataTemp2$year,"-01-01"), origin="1960-01-01", format="%Y-%m-%d")
  DataTemp2$cens <- 0 ## actually, not used in the calculations...
  DataTemp2$timeFix <- 0
  DataTemp2$sex <- 2
  DataTemp2$w <- 1/dim(DataTemp2)[1]  ## or other weights if you can find convenient values to represent the combined distribution of ages at diagnosis and year at diagnosis
  
  for (i in 1000:1000){
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
  SurvExpNew_1[i] <- sum(Temp$surv*Temp$w)
  SurvExpNew_2[i] <- sum(Temp2$surv*Temp2$w)
  
  SurvExpNew_age_cats_men[j,]<-c(j,SurvExpNew_1[1000])
  SurvExpNew_age_cats_women[j,]<-c(j,SurvExpNew_2[1000])
  }
}

SurvExpNew_age_cats_men2<-SurvExpNew_age_cats_men%>%
  as.data.frame()%>%
  rename("age"="V1")%>% #age coded in age groups of five years like globocan
  rename("ES"="V2")%>%
  mutate(sex=1)

Seychelles_expected_Survival<-SurvExpNew_age_cats_women%>%
  as.data.frame()%>%
  rename("age"="V1")%>% #age coded in age groups of five years like globocan
  rename("ES"="V2")%>%
  mutate(sex=2)%>%
  full_join(SurvExpNew_age_cats_men2)
  
write.csv(Seychelles_expected_Survival, "\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\Seychelles_expected_Survival.csv")

