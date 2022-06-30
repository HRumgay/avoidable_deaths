##########################
#
#Script to Combine all SURVCAN life tables into one 
#
##########################

#Life tables - used for analysis
#To clean all names from the _ symbol
life_file_list<-list.files('\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\lifetables\\Expanded_2018', full.names=TRUE)
life<-plyr::ldply(life_file_list,read.dta13)
life<-life%>%as.data.frame()%>%
  clean_names()%>%
  filter(!is.na(mx))

Mauritius<-read.dta13(life_file_list[22])
# Mauritius<-Mauritius%>%clean_names()%>%rename("region"="country")

life<-life%>%full_join(Mauritius)

#Load mortality rates for survival analysis. Correct and use probability
life_table<-life%>%filter(sex==2)%>%
  select(region,year,age,mx,prob)%>%
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
life_table_2019<-life_table%>% filter(year==2015)%>%
  mutate(year=replace(year,year==2015,2019))
life_table_2020<-life_table%>% filter(year==2015)%>%
  mutate(year=replace(year,year==2015,2020))

life_table_complete<-life_table %>%
  full_join(life_table_2019) %>%
  full_join(life_table_2020)


write.csv(life_table_complete,"\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\life_table_SURVCAN.csv", row.names = FALSE)

