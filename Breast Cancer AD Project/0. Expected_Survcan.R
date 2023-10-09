## SURVMARK-2 (ICBP)
# calculation of 5-years colorectal cancer relative survival for each of ICBP countries(2010-14).
# compute how many premature death can be avoided if differences in relative survival reduced.

setwd("C:\\Users\\langseliuso\\Documents\\GitHub\\avoidable_deaths\\Breast Cancer AD Project")

#1# load the required packages.
library("haven") # tosu load stata files.
library("tidyverse") # to manipulate data.
library("lubridate") # to format dates.
library("relsurv") # to estimate relative survival.
library("data.table")

#2# ## Define constants. 
agemin <- 15
agemax <- 99
yearmin <- 2008
yearmax <- 2012
Y2D <- 365.241
Ylen <- 5
CancerSite <- 1
anlPer <- 4
breaks <- c(agemin-1,50,99,agemax+1)
juriscodes <- c("EUNOR10",  "EUIRL10",  "EUUKMALL", "ANCANALL", "OCAUSALL",
                "OCNEZ80",  "EUUKM12",  "EUUKMNIR", "EUUKMSCO", "EUUKMWAL",
                "ANCANNBR", "ANCANBCO", "ANCANALB", "ANCANPEI", "ANCANNFD",
                "ANCANONT", "ANCANSAN", "ANCANNSC", "ANCANMAN", "OCAUSVIC",
                "OCAUSWES", "OCAUSNEW")

Cancer_Lab <- c("Breast")
#c("Oesophagus (C15)","Stomach (C16)","Colon (C18-19)","Rectum (C20)","Liver (C22.0-1)","Pancreas (C25)","Lung (C34)","Ovary (C48.1-2, C56, C57.0)")


#life tables

life_table<-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\life_table_SURVCAN.csv")%>%
  mutate(region = replace(region, region == "Cote d'Ivoire", "C?te d'Ivoire")) %>%
  mutate(region = replace(region, region == "France", "Martinique")) %>%
  mutate(region=replace(region,region=="Korea","South Korea"))%>%
  mutate(region=replace(region,region=="South_Africa","South Africa"))%>%
  mutate(region=replace(region,region=="Cote_D`ivoire","Cote d'Ivoire"))%>%
  mutate(region=replace(region,region=="Saudi_Arabia","Saudi Arabia"))%>%
  mutate(region=replace(region,region=="Costa_Rica","Costa Rica"))%>%
  mutate(region=replace(region,region=="Bahain","Bahrain"))%>%
  mutate(region=replace(region,region=="Costa_Rica","Costa Rica"))%>%
  mutate(region=replace(region,region=="Ethiopy","Ethiopia"))%>%
  left_join(country_codes, by = c("region"="country_label", "country_code"))%>%
  dplyr::rename("country"="region")%>%
#  filter(sex==2)%>%
 select(-country)%>%
  distinct()#%>%
  # filter(age%%5==0)%>% #to match the age groups we choose the 
  # filter(!age %in% c(90,95,100))%>%
  # mutate(agegr = case_when(age >= 15 & age < 65 ~ "15-49",
  #                            age >= 65 & age <= 99 ~ "50-99",
  #                            age<15 ~ "0-15"))


#importing moving around rows to columns for UN population data for PYs
unpop<-read_xlsx("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\Breast Cancer\\Population\\UNpop_readable.xlsx")%>%
  as.data.frame()

unpop0<-unpop%>%
  select(country_code,year,  "0")%>%
  rename("py"="0")%>%
  mutate(age=0)

unpop1<-unpop%>%
  select(country_code,year,  "1")%>%
  rename("py"="1")%>%
  mutate(age=1)

unpop2<-unpop%>%
  select(country_code,year,  "2")%>%
  rename("py"="2")%>%
  mutate(age=2)

unpop3<-unpop%>%
  select(country_code,year,  "3")%>%
  rename("py"="3")%>%
  mutate(age=3)

unpop4<-unpop%>%
  select(country_code,year,  "4")%>%
  rename("py"="4")%>%
  mutate(age=4)

unpop5<-unpop%>%
  select(country_code,year,  "5")%>%
  rename("py"="5")%>%
  mutate(age=5)

unpop6<-unpop%>%
  select(country_code,year,  "6")%>%
  rename("py"="6")%>%
  mutate(age=6)

unpop7<-unpop%>%
  select(country_code,year,  "7")%>%
  rename("py"="7")%>%
  mutate(age=7)

unpop8<-unpop%>%
  select(country_code,year,  "8")%>%
  rename("py"="8")%>%
  mutate(age=8)

unpop9 <- unpop%>%
  select(country_code,year,  "9")%>%
  rename("py"="9")%>%
  mutate(age=9)

unpop10 <- unpop%>%
  select(country_code,year,  "10")%>%
  rename("py"="10")%>%
  mutate(age=10)


unpop11<-unpop%>%
  select(country_code,year,  "11")%>%
  rename("py"="11")%>%
  mutate(age=11)

unpop12<-unpop%>%
  select(country_code,year,  "12")%>%
  rename("py"="12")%>%
  mutate(age=12)

unpop13<-unpop%>%
  select(country_code,year,  "13")%>%
  rename("py"="13")%>%
  mutate(age=13)

unpop14<-unpop%>%
  select(country_code,year,  "14")%>%
  rename("py"="14")%>%
  mutate(age=14)

unpop15<-unpop%>%
  select(country_code,year,  "15")%>%
  rename("py"="15")%>%
  mutate(age=15)

unpop16<-unpop%>%
  select(country_code,year,  "16")%>%
  rename("py"="16")%>%
  mutate(age=16)

unpop17<-unpop%>%
  select(country_code,year,  "17")%>%
  rename("py"="17")%>%
  mutate(age=17)

unpop18<-unpop%>%
  select(country_code,year,  "18")%>%
  rename("py"="18")%>%
  mutate(age=18)

unpop19 <- unpop%>%
  select(country_code,year,  "19")%>%
  rename("py"="19")%>%
  mutate(age=19)

unpop20 <- unpop%>%
  select(country_code,year,  "20")%>%
  rename("py"="20")%>%
  mutate(age=20)

unpop_df<-unpop0%>%
  full_join(unpop1)%>%
  full_join(unpop2)%>%
  full_join(unpop3)%>%
  full_join(unpop4)%>%
  full_join(unpop5)%>%
  full_join(unpop6)%>%
  full_join(unpop7)%>%
  full_join(unpop8)%>%
  full_join(unpop9)%>%
  full_join(unpop10)%>%
  full_join(unpop11)%>%
  full_join(unpop12)%>%
  full_join(unpop13)%>%
  full_join(unpop14)%>%
  full_join(unpop15)%>%
  full_join(unpop16)%>%
  full_join(unpop17)%>%
  full_join(unpop18)%>%
  full_join(unpop19)%>%
  full_join(unpop20)%>%
  mutate(agegr= case_when(age >= 4 & age < 14 ~ "15-49",
                             age >= 14 ~ "50-99",
                             age<4 ~ "0-15"))
  
  
unpop_df$cens <- 0 





men3<-life_table%>%
  filter(sex==1)%>%
  as.data.table()

women3<-life_table%>%
  filter(sex==2)%>%
  as.data.table()




#converting to rate table format. 


ES_list <- lapply(unique(country_codes_life$country_code), function(k) {
  #Looping through the countries
  
  #Aggregating life table data forward and converting to a matrix...
  men4<-men3[country_code==k,]
  women4<-women3[country_code==k,]
  
  men <- matrix(NA, 100, 5, dimnames = list(c(seq(0,99, by = 1)), c(seq(2008, 2012, by = 1))))
   
  women <- matrix(NA, 100, 5, dimnames = list(c(seq(0,99, by = 1)), c(seq(2008, 2012, by = 1))))
  
  
  for(j in 2008:2012){
    for(i in 0:100){
      men2<-men4[year==j,]
      men[i,j-2007] <- men2[i,]$prob

      women2<-women4[year==j,]
      women[i,j-2007] <- women2[i,]$prob
    }
  }
  
  rat.obj <-
    transrate(men, 
              women,
              yearlim = c(2008, 2012),
              int.length = 1) 
  
  
  unpop_df2<-unpop_df%>%
    filter(country_code%in% life_table)
  
  #unpop_df2$year <- as.Date(paste0(unpop_df2$year,"-01-01"),origin="1960-01-01",format="%Y-%m-%d")
  
  nessie(Surv(time,cens)~agegr, rmap=list(age=age*365.241),
         ratetable=rat.obj, data=unpop_df2, times=c(1,2,3,4,5))

  
  })


data(slopop)
data(rdata)
rdata$agegr <-cut(rdata$age,seq(40,95,by=5))
nessie(Surv(time,cens)~agegr,rmap=list(age=age*365.241),
       ratetable=slopop,data=rdata,times=c(1,3,5,10,15))


# 
# #3# data prepration fro cancer cases.
# #3a# clean cancer cases data set and modified to the requirements of the relsurv package.
# modCC <- function(cc) {
#   cc$dob <- cc$dob %>%
#     ymd() # ensure date of birth in the correct format
#   cc$yeardiag <- as.numeric(substr(cc$doi,1,4))
#   cc$month <- as.numeric(substr(cc$doi,5,6))
#   cc$day <- as.numeric(substr(cc$doi,7,8))
#   Idx1 <- which(cc$day=="99")
#   if (length(Idx1)>0){
#     cc[Idx1,]$doi <- paste0(substr(cc[Idx1,]$doi,1,6),"01")
#   } # replace unkown day of incidence with 01 (start of the month)
#   Idx2 <- which(cc$month=="99")
#   if (length(Idx2)>0){
#     cc[Idx2,]$doi <- NA
#   } # exclude cases were month of incidence is unkown.  
#   cc$doi <- cc$doi %>%
#     ymd() # ensure date of incidence is in the correct format. 
#   ccb <- cc %>%
#     filter(age>=agemin & age<agemax & !is.na(cancer_lab) & exclude==0 & multiple==0 & year>=yearmin & year<yearmax) # exlude cases outside of age range and multiples.  
#   ccb$age <- ccb$age*Y2D # age in days.
#   ccb$agegr <- cut(ccb$age/Y2D, breaks, right = FALSE) # create age groups.
#   ccb$sex_old <- ccb$sex
#   if (ccb$sex[1] %in% c("Male","Female")){
#     ccb$sex <- 1 + (ccb$sex_old=="Female")
#   } # ensure that r understand different coding for sex. 
#   new_cc <- ccb %>% # select relevant variables and cases
#     select(regcode, sex, age, dob, doi, period, surv_dd, status, cancer_lab, agegr) %>% 
#     filter(cancer_lab %in% CancerSite) %>%
#     filter(period == anlPer)
#   return(new_cc)
# }
# 
# #3b# extra prepration for countries that have muliple regions. Note it is already combined with modCC.  
# prepCC <- function(cc, juriscodes){
#   if (juriscodes %in% c("EUUKMALL","EUUKM12")){
#     cc$regcode <- paste0(cc$regcode,"_",cc$region)
#     new_cc <- cc %>%
#       modCC()
#   }else { new_cc <- cc %>%
#     modCC
#   }
# }
# 
# #4# data prepration for life tables
# #4a# clean life table and modified to the requirements of the relsurv package.This include renaming variables, selecting years, transform the table into wide formate, and create the ratetable object. 
# modLT <- function(lt){
#   lt.M <- lt %>%
#     subset(sex %in% c(1,"Males")) %>%
#     select("_age", "_year", "prob") %>%
#     rename("age" = "_age") %>%
#     rename("year" = "_year") %>%
#     filter(year %in% c(yearmin:yearmax)) %>%
#     spread(year, prob)
#   lt.M <- as.matrix(lt.M[order(lt.M$age),which(names(lt.M)!="age")])
#   lt.F <- lt %>%
#     subset(sex %in% c(2,"Females")) %>%
#     select("_age", "_year", "prob") %>%
#     rename("age" = "_age") %>%
#     rename("year" = "_year") %>%
#     filter(year%in%c(yearmin:yearmax)) %>%
#     spread(year, prob)
#   lt.F <- as.matrix(lt.F[order(lt.F$age),which(names(lt.F)!="age")])
#   ry <- c(yearmin, yearmax)
#   rat.obj <- transrate(lt.M,lt.F, ry, 1)
#   return(rat.obj)
# }
# 
# #4b# extra prepration for countries that have muliple regions. Mainly create a list of life tables. Note it is already combined with modLT.  
# prepLT <- function(lt, juriscodes){
#   if (juriscodes %in% c("EUUKMALL","EUUKM12")){
#     lt$regcode <- paste0(lt$regcode,"_",lt$region)
#   }
#   if (juriscodes %in% c("EUUKMALL","EUUKM12","ANCANALL","OCAUSALL")){
#     MyListReg <- list()
#     for (i in 1:length(nRegcode)){
#       MyListReg[[nRegcode[i]]] <- modLT(subset(lt,regcode==nRegcode[i]))
#     }
#     rat.obj <- joinrate(MyListReg,dim.name="regcode")
#   }
#   else {
#     rat.obj <- modLT(lt)
#   }
# } 

#5# estimates the number of people alive at 5 years in the reference country or jurisdiction given expected mortality rates
apply_nessie <- function(cc, rat.obj, ansex) {
  fit <- nessie(Surv(surv_dd,status) ~ agegr, data = unpop3, ratetable = rat.obj, times = Ylen, rmap = list(age = age, sex = sex, year = doi))
  return(fit)
}

#6# relative survival functions
#6a# estimations of relative survival 
# rsr <- function(cc, rat.obj, ansex) {
#   fit_rsr <- rs.surv(Surv(surv_dd,status) ~ agegr, data = subset(cc,sex %in% ansex), ratetable = rat.obj, method ="ederer2", rmap = list(age = age, sex = sex, year = doi))
#   return(fit_rsr)
# }
# 
# #6b# Ploting function
# MyPlot <- function(obj,gr,add=FALSE,...){
#   mystr <- c(0,as.numeric(obj$strata))
#   myfirst <- 1 + sum(mystr[1:gr])
#   mylast <- sum(mystr[1:(gr+1)])
#   if (add==FALSE){
#     plot(obj$time[myfirst:mylast],obj$surv[myfirst:mylast],type="l",...)
#   }
#   else {
#     points(obj$time[myfirst:mylast],obj$surv[myfirst:mylast],type="l",...)
#   }
# }
# 
# #7# calculation of colorectal cancer avoidable death. Note ref counttry is the country that has the low survival, whereas, country of interest is the on with the higher survival.  
# Avoidth <- function(rs_ref, rs_intreset, fit_ref) {
#   diff <- summary(rs_intreset, c(Y2D*Ylen))$surv - summary(rs_ref, c(Y2D*Ylen))$surv
#   AD <- fit_ref$mata[2]*diff[c(1,4,3,2)]
#   OD <- fit_ref$mata[2]* (1-summary(rs_ref, c(Y2D*Ylen))$surv)
#   df <- cbind.data.frame(AD, OD)
#   names(df) <- c("Number of avoidable death","Number of death")
#   row.names(df) <- c("15 to 69", "70 to 79", "80 to 89", "90+")
#   round(df, digits = 0)
#   return(df)
# }
# 
# #8# open files
# 
# 
# 
# 
# 
# 
# #9# relative survival and number of people alive at 5 years. 
# #9a# Norway 
# crc_NOR <- EUNOR10_cc %>%
#   prepCC(juriscodes [1])
# ratetableNOR <- popmort_EUNOR10 %>%
#   prepLT(juriscodes [1])
# rsr_NOR <- rsr(crc_NOR, ratetableNOR, c(1,2))
# fit_NOR <- apply_nessie(crc_NOR, ratetableNOR, c(1,2))
# R_NOR <- summary(rsr_NOR, c(Y2D*Ylen))$surv #5 years relative survival ratios
# 
# #9b# Ireland 
# crc_IRL <- EUIRL10_cc %>%
#   prepCC(juriscodes [2])
# ratetableIRL <- popmort_EUIRL10 %>%
#   prepLT(juriscodes [2])
# rsr_IRL <- rsr(crc_IRL, ratetableIRL, c(1,2))
# fit_IRL <- apply_nessie(crc_IRL, ratetableIRL, c(1,2))
# R_IRL <- summary(rsr_IRL, c(Y2D*Ylen))$surv #5 years relative survival ratios
# 
# #9c# Ireland 
# crc_UK <- EUUKMALL_cc %>%
#   prepCC(juriscodes [3])
# nRegcode <- names(table(crc_UK$regcode))
# ratetableUK <- popmort_EUUKMALL %>%
#   prepLT(juriscodes [3])
# rsr_UK <- rsr(crc_UK, ratetableUK, c(1,2))
# fit_UK <- apply_nessie(crc_UK, ratetableUK, c(1,2))
# R_UK <- summary(rsr_UK, c(Y2D*Ylen))$surv #5 years relative survival ratios
# 
# #9d# crc_CAN <- ANCANALL_cc %>%
# crc_CAN <- ANCANALL_cc %>%
#   prepCC(juriscodes [4]) %>% subset(regcode!="ANCANQUE")
# nRegcode <- names(table(crc_CAN$regcode))
# ratetableCAN <- popmort_ANCANALL %>%
#   prepLT(juriscodes [4])
# rsr_CAN <- rsr(crc_CAN, ratetableCAN, c(1,2))
# fit_CAN <- apply_nessie(crc_CAN, ratetableCAN, c(1,2))
# R_CAN <- summary(rsr_CAN, c(Y2D*Ylen))$surv #5 years relative survival ratios
# 
# #9e# Australia 
# crc_AUS <- OCAUSALL_cc %>%
#   prepCC(juriscodes [5])
# nRegcode <- names(table(crc_AUS$regcode))
# ratetableAUS <- popmort_OCAUSALL %>%
#   prepLT(juriscodes [5])
# rsr_AUS <- rsr(crc_AUS, ratetableAUS, c(1,2))
# fit_AUS <- apply_nessie(crc_AUS, ratetableAUS, c(1,2))
# R_AUS <- summary(rsr_AUS, c(Y2D*Ylen))$surv # 5 years relative survival ratios
# 
# #9f# New Zealand
# crc_NZ <- OCNEZ80_cc %>%
#   prepCC(juriscodes [6])
# ratetableNZ <- popmort_OCNEZ80 %>%
#   prepLT(juriscodes [6])
# rsr_NZ <- rsr(crc_NZ, ratetableNZ, c(1,2))
# fit_NZ <- apply_nessie(crc_NZ, ratetableNZ, c(1,2))
# R_NZ <- summary(rsr_NZ, c(Y2D*Ylen))$surv # 5 years relative survival ratios 
# 
# #10# compare the 5 years relative survival ratios bwtween ICBP countries.
# RS <- data.frame(Country = c("Norway", "Ierland", "UK", "Canada", "Australia", "New Zealand"), Number_of_cases = c(rsr_NOR$n[1], rsr_IRL$n[1], rsr_UK$n[1], rsr_CAN$n[1], rsr_AUS$n[1], rsr_NZ$n[1]), Relative_Survival = c(R_NOR[1], R_IRL[1], R_UK[1], R_CAN[1], R_AUS[1], R_NZ[1]))
# write.csv(RS, "I:/Xchange/Hazem/SURVMARK-2 Avoidable Death/relative survival.csv")
# 
# ### plot
# Myg <- 1
# MyPlot(rsr_NOR,gr=Myg,lwd = 2,col="#377EB8",ylim=c(0,1), main = "5 year relative survival,Ovary (C48.1-2, C56, C57.0)", sub = "SURVMARK-2", ylab="Survival Probability", xlab="Time in days")
# MyPlot(rsr_IRL,gr=Myg,add = TRUE,lwd=2,col="#FF7F00")
# MyPlot(rsr_UK,gr=Myg,add = TRUE,lwd=2,col="#E41A1C")
# MyPlot(rsr_CAN,gr=Myg, add = TRUE,lwd=2,col="#4DAF4A")
# MyPlot(rsr_AUS,gr=Myg, add = TRUE,lwd=2,col="#984EA3")
# MyPlot(rsr_NZ,gr=Myg, add = TRUE,lwd=2,col="#F781BF")
# 
# #11# calculation of the number of death that can be avoided 
# AD_NOR <- Avoidth(rsr_NOR, rsr_NOR, fit_NOR)
# AD_IRL <- Avoidth(rsr_IRL, rsr_NOR, fit_IRL)
# AD_UK <- Avoidth(rsr_UK, rsr_NOR, fit_UK)
# AD_CAN <- Avoidth(rsr_CAN, rsr_NOR, fit_CAN)
# AD_AUS <- Avoidth(rsr_AUS, rsr_NOR, fit_AUS)
# AD_NZ <- Avoidth(rsr_NZ, rsr_NOR, fit_NZ)
# 
# AD <- cbind.data.frame(AD_NOR, AD_IRL, AD_UK, AD_CAN, AD_AUS, AD_NZ)
# #write.csv(AD, "I:/Xchange/Hazem/SURVMARK-2 Avoidable Death/Avoidable death.csv")
