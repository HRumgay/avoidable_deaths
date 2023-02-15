library(stringr)

# globocan <- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Globocan2020\\Globocan.csv", 
#             stringsAsFactors = FALSE)%>%
#   filter(country_label%in%c("India","United States of America"))%>%
#   filte


PAFs <-read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\Data\\combinedPAFs_cases_12.07.22.csv")%>%
  group_by(country_code, #sex,
           cancer_code, age)%>%
  filter(country_label%in%c("United States of America", "India"))%>%
  filter(sex!=0)%>%
  dplyr::mutate(af.comb= case_when(cases!=0 ~ sum(cases.prev)/sum(cases),
                                 cases==0 ~ af.comb))%>%
  ungroup()%>%
  as.data.frame()%>%
  distinct()

PAFs_india<-PAFs%>%
  ungroup()%>%
   select(-age, -sex)%>%
  select(country_code, country_label, cancer_code, cases)%>%
  group_by(country_code,
           cancer_code)%>%
  mutate(cases=sum(cases))%>%
  ungroup()%>%
  distinct()
  

check<- countries_5y%>%
  distinct()%>%
  dplyr::left_join(PAFs_india)%>%
  ungroup()%>%
  select(country_code,country_label, cancer_code, cancer_label, 
         age , rel_surv, cases )%>%
  # mutate(cancer_code = replace(cancer_code, cancer_code == 9, 38))%>%
  # mutate(cancer_code = replace(cancer_code, cancer_code == 8, 38))%>%
  # mutate(cancer_label = replace(cancer_label, cancer_label == "Colon", "Colorectal")) %>%
  # mutate(cancer_label = replace(cancer_label, cancer_label == "Rectum", "Colorectal")) %>%
  mutate(age_cat = case_when(
      age>=4 & age<14 ~ "15-64",
      age>=14 ~ "65-99",
      age<4 ~"0-15"
    ))%>%
  filter(age_cat!="0-15")%>%
  mutate(age_cat="Overall")%>%
  select(-age)%>%
  filter(cases!=0)%>%
  group_by(country_code, cancer_code)%>%
  mutate(rel_surv=sum(rel_surv*cases, na.rm=T)/sum(cases, na.rm=T))%>%
  select(-cases)%>%
  as.data.frame()%>%
  distinct()


survcan_list<-list.files('\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\SURVCAN non age standardized\\', full.names=TRUE)
survcan_ns_non_agestand<-plyr::ldply(survcan_list,read.dta13)

survcan_ns_non_agestand2 <- survcan_ns_non_agestand%>%
  as.data.frame()%>%
  filter(time==5)%>%
  filter(country=="India")%>%
  select(country,cancername,cancer_num,cns,registryname)%>%
  rename("registry"="registryname")%>%
  rename("country_label"="country")%>%
  rename("cancer_code"="cancer_num")%>%
  rename("rel_surv"="cns") %>% 
  mutate(registry = str_remove(registry, "India, "))%>%
 mutate(cancer_code= case_when( cancer_code ==   13 ~ 30,
                                cancer_code == 9~  20,
                                cancer_code == 10~23,
                                cancer_code == 5~8,
                                cancer_code == 15~ 36,
                                cancer_code == 7~11,
                                cancer_code == 8~ 15,
                                cancer_code == 2~ 4,
                                cancer_code ==14~ 34, 
                                cancer_code ==3~6, 
                                cancer_code ==1~ 1, 
                                cancer_code ==11~ 25, 
                                cancer_code ==6~9, 
                                cancer_code ==4~ 7, 
                                cancer_code ==12~ 27
                                
                                )) 
   
     Cancer_codes


Survcan_website_OS <- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\India Example\\India_surv.csv")%>%
  group_by(cancer_code)%>%
  as.data.frame()%>%
  rename("rel_surv_AS"="obs_surv") %>% #columns were swapped by mistake so as a quick fix
  rename("obs_surv"="rel_surv") %>% #columns were swapped by mistake so as a quick fix
  #  filter(Sex=="Both sexes")%>%
 #select(-Sex)%>%
  mutate(obs_surv=obs_surv/100)%>%
  left_join(survcan_ns_non_agestand2, by=c("registry", "cancer_code","country_label"))%>%
  mutate(ES=obs_surv/rel_surv)
  filter(!is.na(rel_surv))
  


# Malaysia seems like a good reference here. China and Turkey seem like they could be political
# 





#max reference in India 

Reference_India_max<-Survcan_website_OS%>%
  group_by(cancer_code) %>%
  slice_max(rel_surv,n=1)%>%
  distinct()%>%
  mutate(surv_ref=case_when(rel_surv >1 ~ 1,
                            rel_surv <=1 ~ rel_surv))%>%
  dplyr::rename("ref_india"="rel_surv")%>%
  select(cancer_code,ref_india)
  


check<- countries_5y%>%
  left_join(PAFs)%>%
  ungroup()%>%
  select(country_code,country_label, cancer_code, cancer_label,
         age , rel_surv, cases )%>%
  # mutate(cancer_code = replace(cancer_code, cancer_code == 9, 38))%>%
  # mutate(cancer_code = replace(cancer_code, cancer_code == 8, 38))%>%
  # mutate(cancer_label = replace(cancer_label, cancer_label == "Colon", "Colorectal")) %>%
  # mutate(cancer_label = replace(cancer_label, cancer_label == "Rectum", "Colorectal")) %>%
  mutate(
    age_cat = case_when(
      age>=4 & age<14 ~ "15-64",
      age>=14 ~ "65-99",
      age<4 ~"0-15"
    ))%>%
  filter(age_cat!="0-15")%>%
  mutate(age_cat="Overall")%>%
  select(-age)%>%
  filter(cases!=0)%>%
  group_by(country_code, cancer_code)%>%
  mutate(rel_surv=sum(rel_surv*cases, na.rm=T)/sum(cases, na.rm=T))%>%
  select(-cases)%>%
  as.data.frame()%>%
  distinct()


Reference_USA <-  check%>%
  filter(country_label=="United States of America")%>%
  as.data.frame()%>%
  select(
          cancer_code,
          rel_surv)%>%
  dplyr::rename("surv_ref"="rel_surv")%>%
  distinct()%>%
  mutate(surv_ref=case_when(surv_ref >1 ~ 1,
                            surv_ref <=1 ~ surv_ref))%>%
  dplyr::rename("ref_USA"="surv_ref")



India_data<-Survcan_website_OS%>%
  left_join(Reference_USA, by=c("cancer_code"))%>%
  left_join(Reference_India_max, by=c("cancer_code"))%>%
  left_join(PAFs_india, by=c("country_code","country_label","cancer_code"))%>%
  filter(Type!="National")
  
# If using the 
# PAF_India<-Simulated_Data_PAF_1%>%
#   filter(country_label=="India")%>%
#   dplyr::mutate(rel_surv=as.double(rel_surv))%>%
#   dplyr::mutate(af.comb=as.double(af.comb))%>% 
#   dplyr::mutate(total_overall=as.double(total_overall))%>%
#   arrange(country_label,cancer_code,age,sex)%>%
#   left_join(Reference_USA, by=c("age","cancer_code"))%>%
#               filter(cancer_label%in%c("Prostate","Breast", "Stomach", "Stomach", "Liver", 
#                                        "Trachea, bronchus and lung", "Liver and intrahepatic bile ducts",
#                                        "Cervix uteri", "Non-Hodgkin lymphoma",
#                                        "Bladder", "Rectum", "Colon", "Lip, oral cavity",
#                                        "Nasopharynx", "Ovary", "Leukaemia",
#                                        "Oesophagus"))%>%
#   
#   
# 
# PAFs_india
#  
# PAF_India_USA <- Simulated_Data_PAF_1%>%
#   select(country_code,country_label,age, sex, 
#          cancer_code,cancer_label,
#          total_overall)%>%
#   filter(country_label=="India")%>%
#   dplyr::mutate(total_overall=as.double(total_overall))%>%
#   arrange(country_label,cancer_code,age,sex)%>%
#   filter(cancer_label%in%c("Prostate","Breast", "Stomach", "Stomach", "Liver",
#                            "Trachea, bronchus and lung", "Liver and intrahepatic bile ducts",
#                            "Cervix uteri", "Non-Hodgkin lymphoma",
#                            "Bladder", "Rectum", "Colon", "Lip, oral cavity",
#                            "Nasopharynx", "Ovary", "Leukaemia",
#                            "Oesophagus"))

# Avoidable deaths

# Three AD calcs 

# first need to make sure data is in right format (numeric columns)

# Applying the equation from Rutherford 2015 for AD.


Avoidable_Deaths_Simulated_All3_india <- India_data%>%
  dplyr::group_by(registry, cancer_code)%>%
  mutate(ES=case_when(   ES<=1~ ES,
                         ES>1~1))%>%
  dplyr::mutate(pAD_treat_USA=  (ref_USA-rel_surv) * ES)%>%
  dplyr::mutate(pAD_treat_India= (ref_india-rel_surv) * ES)%>% # Rutherford model
  dplyr::mutate(AD_treat=cases*0.5 * (ref_USA-rel_surv) * ES +
                  cases*0.5 * (ref_india-rel_surv) * ES)%>%
  mutate(pAD_treat_India=case_when(pAD_treat_India>=0 ~ pAD_treat_India,
                         pAD_treat_India<0~0))%>%
  mutate(pAD_treat_USA=case_when(pAD_treat_USA>=0 ~ pAD_treat_USA,
                      pAD_treat_USA<0~0))


#AD proportion for when lowest survival in india is max in India
india_max_india <- Avoidable_Deaths_Simulated_All3_india%>%
  group_by(cancer_code)%>%
  slice_min(rel_surv)%>%
  distinct()%>%
  select(-pAD_treat_USA)%>%
  select(country_code, country_label, cancer_code, pAD_treat_India,cases)%>%
  ungroup()

india_max_india2 <-Avoidable_Deaths_Simulated_All3_india%>%
  group_by(cancer_code)%>%
  slice_min(rel_surv)%>%
  distinct()%>%
  dplyr::mutate(total_deaths = (1-(rel_surv*ES))*cases)%>%
  select(-pAD_treat_USA)%>%
  ungroup()%>%
  mutate(cancer_code=1000)%>%
  mutate(cancer_label="Fifteen Cancer Sites")%>%
  dplyr::mutate(total_deaths=(1-(rel_surv*ES))*cases)%>%
  mutate(pAD_treat_India=sum(cases*pAD_treat_India)/sum(total_deaths))%>%
mutate(cases=sum(cases))%>%
  select(-total_deaths)%>%
  select(country_code, country_label, cancer_code, pAD_treat_India,cases)%>%
  distinct()

india_max_india3<-india_max_india%>%
  full_join(india_max_india2)


#AD proportion when median in india is that of the USA
india_median_USA <- Avoidable_Deaths_Simulated_All3_india%>%
  as.data.frame()%>%
  ungroup()%>%
  group_by(cancer_code)%>%
  mutate(med_surv=median(rel_surv))%>%
  mutate(med_ES=median(ES))%>%
    dplyr::mutate(pAD_treat_USA= (ref_USA-med_surv) * med_ES)%>% # Rutherford model
  ungroup()%>%
  select(-pAD_treat_India)%>%
  select(country_code, country_label, cancer_code, pAD_treat_USA,cases)%>%
  distinct()
#
india_median_USA2 <- Avoidable_Deaths_Simulated_All3_india%>%
  as.data.frame()%>%
  ungroup()%>%
  group_by(cancer_code)%>%
  mutate(med_surv=median(rel_surv))%>%
  mutate(med_ES=median(ES))%>%
  dplyr::mutate(total_deaths = (1-(rel_surv*ES))*cases)%>%
  dplyr::mutate(pAD_treat_USA = (ref_USA-med_surv) * med_ES)%>% # Rutherford model
  ungroup()%>%
  select(-pAD_treat_India)%>%

  distinct()%>%
  mutate(cancer_code=1000)%>%
  mutate(cancer_label="Fifteen Cancer Sites")%>%
  distinct()%>%
  mutate(pAD_treat_USA=sum(cases*pAD_treat_USA)/sum(total_deaths))%>%
  distinct()%>%
  select(-total_deaths)%>%
  mutate(cases=sum(cases))%>%
  select(country_code, country_label, cancer_code, pAD_treat_USA)%>%
  distinct()%>%mutate(cases=sum(india_max_india$cases))

india_median_USA3 <-india_median_USA %>%
  full_join(india_median_USA2 )

sum(india_median_USA$cases)
sum(india_max_india$cases)

AD_india<-india_median_USA3%>%
  left_join(india_max_india3)%>%
  mutate(AD_treat = 0.5*cases*pAD_treat_USA + 0.5*cases*pAD_treat_India)%>%
  left_join(Cancer_codes, by =c("cancer_code"))%>%
  mutate(cancer_label = replace(cancer_label, cancer_label == "Liver and intrahepatic bile ducts", "Liver"))%>%
  mutate(cancer_label = replace(cancer_label, cancer_label == "Trachea, bronchus and lung", "Lung"))%>%
  select(-icd)%>%
  distinct()%>%
  select(country_code, country_label, cancer_code,cancer_label, 
         cases,pAD_treat_USA,pAD_treat_India, AD_treat)%>%
  dplyr::mutate(across(8:8,round, -1))%>%
  dplyr::mutate(across(6:7, round,3)*100)%>%
  mutate(cancer_label= case_when(cancer_code ==   1000 ~ "Fifteen Cancer Sites",
                                          cancer_code != 1000 ~  cancer_label))


write.csv(AD_india, "C:\\Users\\langseliuso\\OneDrive - IARC\\India_example.csv")


#Data by cancer site prep
AD_by_cancer_site_india <-AD_india

AD_by_cancer_site_3_india<-AD_by_cancer_site_india%>%
  select(cancer_label, cancer_code, pAD_treat_India)%>%
  filter(cancer_code!=1000)%>%
  rename("pAD"="pAD_treat_India")%>%
  dplyr::mutate(AD_cat="Scenario 1")

AD_by_cancer_site_1_india<-AD_by_cancer_site_india%>%
  select(cancer_label, cancer_code, pAD_treat_USA)%>%
  filter(cancer_code!=1000)%>%
  rename("pAD"="pAD_treat_USA")%>%
  dplyr::mutate(AD_cat="Scenario 2")

AD_by_cancer_site_2_india<-AD_by_cancer_site_1_india%>%
  full_join(AD_by_cancer_site_3_india)

  
  
  
nAD_by_cancer_site_india <- AD_by_cancer_site_india%>%
  filter(cancer_code!=1000)%>%
  select(cancer_label, cancer_code, AD_treat)%>%
  #rename("AD"="AD_treat")%>%
  dplyr::mutate(AD_cat="Avoidable") # Number Treatable Avoidable Deaths if 50% of cases are in first scenario and 50% in second

#plotting them 
library(ggpubr)
pAD_india_1 <- AD_by_cancer_site_3_india %>%
  ggplot(
    aes(cancer_label, pAD, fill="AD_cat",
        ymin = 0,
        ymax = 100),
    mapping = aes(
      reorder(cancer_label, pAD),pAD,drop=FALSE, fill=AD_cat,
    )) +
  xlab("Cancer Site") +
  ylab("Proportion Treatable Avoidable Deaths (pAD, %)") +
  ggtitle("Proportion Treatable Avoidable Deaths in India for Fifteen Cancer Sites in SURVCAN-3
           Scenario 2: if the registry with the lowest relative survival in India has survival as the highest in India)") +
 scale_fill_manual(values = c('#de2d26','#fc9272',  '#fee0d2')) + #choose some nice colours for your bars
  geom_bar(stat = "identity", 
           position = "dodge", fill="blue") +
  geom_hline(yintercept = AD_india[16,]$pAD_treat_India, color="red")+
  annotate("text", x=3, y=25, label= "pAD, all cancer sites combined") +
  theme_light()+
  coord_flip()


pAD_india_2 <- AD_by_cancer_site_1_india %>%
  ggplot(
    aes(cancer_label, pAD, fill="AD_cat", 
        ymin = 0,
        ymax = 100),
    mapping = aes(
      reorder(cancer_label, pAD),pAD,drop=FALSE, fill=AD_cat
    )) +
  xlab("Cancer Site") +
  ylab("Proportion Treatable Avoidable Deaths (pAD, %)") +
  ggtitle("Proportion Treatable Avoidable Deaths in India for Fifteen Cancer Sites in SURVCAN-3
          Scenario 1: if the median net survival in India was increased to that of the USA") +
 # scale_fill_manual(values = c('#fc9272',  '#fee0d2')) + #choose some nice colours for your bars
  geom_bar(stat = "identity", 
           position = "dodge", fill="blue")  +
  geom_hline(yintercept = AD_india[16,]$pAD_treat_USA, color="red")+
  annotate("text", x=3, y=28, label="pAD, all cancer sites combined") + 
theme_light()+
  coord_flip()


AD_by_cancer_site_1_india %>% 
  ggdotchart(x = "cancer_label", y = "pAD",
           color = "red",                             # Color by groups
           palette = c( "#FC4E07"), # Custom color palette    "#00AFBB", "#E7B800",
           sorting = "descending",                       # Sort value in descending order
           add = "segments",                             # Add segments from y = 0 to dots
           rotate = TRUE,                                # Rotate vertically
           group = "AD_cat",                                # Order by groups
           dot.size = 12,                                 # Large dot size
           label = round(AD_by_cancer_site_1_india$pAD,1),                        # Add mpg values as dot labels
           font.label = list(color = "white", size = 10, 
                             vjust = 0.5),               # Adjust label parameters
           xlab="Cancer Site", 
           ylab="Proportion Treatable Avoidable Deaths (pAD, %)",
           title="Proportion Treatable Avoidable Deaths in India for Fifteen Cancer Sites in SURVCAN-3, Scenario 1: if the median net survival in India was increased to that of the USA",
           ggtheme = theme_pubr()                        # ggplot2 theme
) ->pAD_india_2_alt_plot


#pAD_india_2_alt_plot$sp <- 
  
  # pAD_india_2_alt_plot$sp + 
  # geom_hline(yintercept = AD_india[16,]$pAD_treat_USA, color="black")+
  # annotate("text", x=3, y=28, label= "Fifteen Cancer Sites Proportion")
  # 
  # 
pAD_india_2_alt_plot
  

  



pAD_india_1
pAD_india_2

ggsave("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\India Example\\Figures\\india_2.pdf", width = 10, height =10, limitsize = FALSE,plot=pAD_india_1) 
ggsave("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\India Example\\Figures\\india_1.pdf", width = 10, height =10, limitsize = FALSE,plot=pAD_india_2) 
ggsave("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\India Example\\Figures\\india_1_alt.pdf", width = 10, height =10, limitsize = FALSE,plot=pAD_india_2_alt_plot) 




# Number AD Pie Chart

# load libraries needed for some charts
library(plyr) #added plyr here for rounding for the pie charts but it might break some tidyverse functions
library(tidyverse)
library(data.table)
library(Rcan)
library(ggrepel)
library("ggplot2")
library(ggsci)
library(grid)
text_high <- textGrob("Highest\nvalue", gp=gpar(fontsize=13, fontface="bold"))
text_low <- textGrob("Lowest\nvalue", gp=gpar(fontsize=13, fontface="bold"))


col <- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Research visits\\Oliver_Langselius\\cancer_color_2018.csv")

view(col)

nAD_by_cancer_site_india %>% 
  group_by(cancer_label) %>% 
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T)) %>% 
  filter(!is.na(cancer_code)) %>% 
  dplyr::select(cancer_label,cancer_code,AD_treat)%>% 
  unique() %>% 
  left_join(col %>% 
              select(cancer_label:Color.Hex) %>%
              filter(cancer_label!="Colorectum")) %>% 
  pivot_longer(AD_treat:AD_treat,
               names_to="AD_cat",
               values_to = "AD") %>%
  group_by(AD_cat) %>% 
  dplyr::mutate(percent=sum(AD),
                percent = AD/percent) %>% 
  dplyr::arrange(AD) %>% 
  dplyr::mutate(rankc = as.numeric(dplyr::row_number())) %>% 
  group_by(AD_cat) %>% 
  dplyr::select(cancer_label,Color.Hex,AD_cat,AD,percent,rankc) %>% 
  unique() -> AD_by_cancer_site_1

#treatable pie
piedt <- as.data.table(AD_by_cancer_site_1 %>%filter(AD_cat=="AD_treat") )
piedt %>%
  ggplot(aes(x = 2, y = percent, fill = factor(rankc,levels = unique(piedt$rankc),
                                               labels = unique(piedt$Color.Hex)),
             width=2)) +
  geom_bar(width = 1, stat = "identity") +
  geom_text(aes(label = paste0(formatC(round_any(AD,100), format="f", big.mark=",", digits=0),"\n ", 
                               scales::percent(percent, accuracy = 1)), x = 2.75),
            position = position_stack(vjust=0.5),
            size=3) +
  coord_polar(theta = "y") +
  scale_fill_identity("Cancer site", labels = piedt$cancer_label,
                      guide = "legend") +
  guides(fill = guide_legend(reverse = TRUE))+
  theme(plot.background= element_blank(),
        plot.title = element_text(size=12, margin=margin(0,0,0,0),hjust = 0.5),
        panel.background = element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        panel.border = element_blank(),
        panel.grid = element_blank(),
        axis.ticks = element_blank(),
        strip.background = element_blank(),
        plot.caption = element_text(hjust = 0.5, face = "italic"))+# move caption to the left)+
 # theme(legend.position = "none")+ 
  labs(title="Treatable Avoidable Deaths in India", caption= paste(formatC(round( AD_india[16,]$AD_treat,-3), format="d", big.mark=",")," total deaths"))-> pie.treat
pie.treat


ggsave("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\India Example\\Figures\\india_pie.pdf", width = 10, height =10, limitsize = FALSE,plot=pie.treat) 

AD_india

