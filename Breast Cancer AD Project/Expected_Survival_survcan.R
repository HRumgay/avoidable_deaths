##########################################################
#
# Expected survival for all countries, ages and sex 19/01/23
#
##########################################################

#*the following script supersedes previous expected survival scripts*

# run following libraries
library("data.table")

# clear environment
rm(list = ls())

# ---- data needed in file ----

# load HDI data to use for missing countries
load("data/HDI.RData")
load("country_codes.RData")
HDI %>% 
  select(-country_label) %>% 
  left_join(country_codes) %>% 
  filter(!is.na(country_label))->HDI

# load lifetables file including country labels
load("data/popmort2.RData")
# popmort2 is WHO life tables for 2000, 2005, 2010, 2015, 2019
# by 19 age groups (<1, 1-4, 5-9, ... 85+) for 186 countries
# available at https://apps.who.int/gho/data/node.main.LIFECOUNTRY?lang=en
# ndx - number of people dying between ages x and x+n 
# nLx - person-years lived between ages x and x+n   = (lx-ndx/2)*n

# reformat as data.table
p <- data.table(popmort2)

# remove both sexes combined and calculate mortality rate per age ndx/nLx
p <- p[sex>0,][,nMx:=ndx/nLx] # calculate age-specific mortality rate


#---- expected survival assuming diagnosed in middle of age group: ----
# 5 year surv = exp(-5/2*mort(agei) - 5/2*mort(agei+1))
p$es <- 0 # create expected survival variable
# calculate expected survival for ages 5-9 to 80-84
for (i in 2:17){
  p[age==i,es:=exp(-2.5*nMx-2.5*p[age==(i+1)]$nMx)]
}
# calculate expected survival for age <1
p[age==0, es:=exp(-0.5*nMx-4.5*p[age==1]$nMx)]
# calculate expected survival for age 1-4
p[age==1, es:=exp(-2*nMx-3*p[age==2]$nMx)]
# calculate expected survival for age 85+
p[age==18, es:=exp(-5*nMx)]


#---- expected survival assuming diagnosed at beginning of of age group: ----
# assume diagnosed at beginning of age group:
# 5 year surv = exp(-5*mort(agei))

p$es2 <- 0 # create expected survival variable
# calculate expected survival for ages 5-9 to 80-84
for (i in 2:17){
  p[age==i,es2:=exp(-5*nMx)]
}
# calculate expected survival for age <1
p[age==0, es2:=exp(-1*nMx-4*p[age==1]$nMx)]
# calculate expected survival for age 1-4
p[age==1, es2:=exp(-4*nMx-1*p[age==2]$nMx)]
# calculate expected survival for age 85+
p[age==18, es2:=exp(-5*nMx)]


#---- combine expected survival for ages <1 and 1-4 using nLx to weight estimates----

# create weight variable
p$w<-1

# create weights between ages 0 and 1
p[age %in% c(0,1),w:=nLx/sum(nLx),by=list(sex, country_code, year)]

# replace es in age group 1 with weighted es of age groups 0 and 1
p[age==1,es:=(es*w)+(p[age==0]$es*p[age==0]$w)]
p[age==1,es2:=(es2*w)+(p[age==0]$es2*p[age==0]$w)]

# remove age group 0, age group 1 is now es for 0-4. Remove weighting for ages 0+1
p <- p[age>0,][,w:=NULL][,ndx:=NULL][,nLx:=NULL][,nMx:=NULL]


#---- fill in es for 9 missing countries ----

# 9 globocan countries not included in life tables
# Gaza Strip and West Bank 275
# French Guiana 254
# French Polynesia 258
# France, Guadeloupe 312
# Guam 316
# France, Martinique 474
# France, New Caledonia 540
# Puerto Rico 630
# France, La Réunion 638

# Use France es for For all the French overseas territories (French Polynesia, 
# French Guiana, Guadeloupe, Martinique, La Réunion, New Caledonia)
# Use USA for Puerto Rico and Guam
p %>% 
  rbind(p %>% 
          filter(country_code==250) %>% 
          mutate(country_code=254)) %>%  #French Guiana
  rbind(p %>% 
          filter(country_code==250) %>% 
          mutate(country_code=258))%>% #French Polynesia
  rbind(p %>% 
          filter(country_code==250) %>% 
          mutate(country_code=312))%>% #Guadeloupe
  rbind(p %>% 
          filter(country_code==250) %>% 
          mutate(country_code=474))%>% #Martinique
  rbind(p %>% 
          filter(country_code==250) %>% 
          mutate(country_code=638))%>% #La Réunion
  rbind(p %>% 
          filter(country_code==250) %>% 
          mutate(country_code=540))%>% #New Caledonia
  rbind(p %>% 
          filter(country_code==840) %>% 
          mutate(country_code=630))%>% #Puerto Rico
  rbind(p %>% 
          filter(country_code==840) %>% 
          mutate(country_code=316)) %>% #Guam
  rbind(p %>% 
          filter(country_code==840) %>% 
          mutate(country_code=275)) %>% #add rows for Gaza strip - to replace in next lines
  select(-country_label) %>% #remove country labels to be replaced with correct labels
  full_join(HDI %>% select(country_code,country_label,hdi_value))->p

p<- p[country_code==275, es:=mean(p[(hdi_value>=0.690-0.05&hdi_value<=0.690+0.05)]$es), by=list(sex,age,year)]
p<- p[country_code==275, es2:=mean(p[(hdi_value>=0.690-0.05&hdi_value<=0.690+0.05)]$es2), by=list(sex,age,year)]

p$hdi_value<-NULL


#---- save file ----
#save(p,file ="\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\ExpectedSurvival.RData")


