# projection purely based on population growth and aging 


# UN population projections by country by age and sex


UN_Pop_Projections <- read.csv("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024\\Data\\UNDP_population_2024.csv")%>%
  as.data.frame()

# converting to long form and filtering the right years (2024-2055)

UN_long <- UN_Pop_Projections %>%
  pivot_longer(
    cols      = num_range("age", 1:20),
    names_to  = "age",
    names_prefix = "age",
    values_to = "py_proj"
  ) %>%
  mutate(age = as.integer(age))%>%
  dplyr::select(-age80p, -age100p)%>%
  mutate(age=case_when(age==1~ 1,
                       age==2~ 2,
                       age==3~ 3,
                       age==4~ 4,
                       age>4 & age<9 ~ 4,
                       age==9 ~ 9,
                       age==10 ~ 10,
                       age==11 ~ 11,
                       age==12 ~ 12,
                       age==13 ~ 13,
                       age==14 ~ 14,
                       age==15 ~ 15,
                       #age>=16 ~ 16,
                       age==16 ~ 16,
                       age==17 ~ 17,
                       age==18 ~ 18,
                       age==19 ~ 18,
                       age==20 ~ 18))%>%
  ungroup()%>%
  distinct()%>%
  group_by(year, country_code, sex, age)%>%
  mutate(py_proj=sum(py_proj)*1000)%>%
  ungroup()%>%
  distinct()%>%
  dplyr::filter(sex%in%c(1,2))%>%
  as.data.frame()%>%
  dplyr::select(-country_label)%>%
  filter(age>3)


#checking country_labels
UN_labels<-UN_Pop_Projections %>% dplyr::select(country_code, country_label)%>%distinct()

AD_labels<-Avoidable_Deaths_Simulated_All3 %>%ungroup()%>% dplyr::select(country_code, country_label)%>%distinct()


Mismatched_countries<-AD_labels%>%
  left_join(UN_labels, by=c("country_code"))


# since UN file has a bit more countries and territories filter to only 
# 185/ 186 AD countries -- need to add Curacao 

UN_long <-UN_long%>%
  filter(country_code %in% AD_labels$country_code)


UN_long

#PY 2024 to scale the ADs 

py_2024 <- UN_long%>%filter(year==2024)%>%dplyr::select(country_code, sex, age, py_proj)%>%
  rename("py_2024"= "py_proj")%>%
  distinct()



# joining avoidable deaths by country 
AD_projections <- UN_long%>%
  left_join(Avoidable_Deaths_Simulated_All3%>%
              dplyr::filter(cancer_code%in%c(7))%>%
              dplyr::select(-py),
            by=c("country_code","sex", "age"))%>%
  left_join(py_2024, by=c("country_code","sex", "age"))%>%
  left_join(weights2, by=c("age"))%>%
  filter(year%in%c(2024:2055))%>%
  filter(!is.na(country_code))%>%
  #calculating the projected ADs by year
  dplyr::mutate(AD_treat_prev=AD_treat + AD_prev)%>%
  dplyr::mutate(scale = py_proj/py_2024)%>%
  dplyr::mutate(AD_prev       = AD_prev*scale,
                AD_treat      = AD_treat*scale,
                AD_treat_prev = AD_treat_prev*scale,
                AD_unavoid    = AD_unavoid*scale,
                total_deaths2 = total_deaths2*scale,
                total_overall = total_overall*scale)%>%
  dplyr::mutate(AD_sum = AD_prev + AD_unavoid + AD_treat)%>%
  dplyr::mutate(AD_treat = case_when(AD_treat<0 ~ 0,
                                     TRUE ~ AD_treat))%>%
  dplyr::rename("py"="py_proj")%>%
  dplyr::select(-scale, -py_2024)%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()

# sanity: 2024 must reproduce the source
sum(AD_projections$total_deaths2[AD_projections$year==2024])
sum(Avoidable_Deaths_Simulated_All3$total_deaths2[
  Avoidable_Deaths_Simulated_All3$cancer_code==7], na.rm=TRUE)


# ---------------------------------------------------------------------------
# Both sexes combined
# ---------------------------------------------------------------------------
AD_projections_overall<-AD_projections%>%
  mutate(sex=0)%>%
  dplyr::group_by(year, country_code, cancer_code, sex, age)%>%
  dplyr::mutate(AD_prev=sum(AD_prev),
                AD_treat=sum(AD_treat),
                AD_treat_prev=sum(AD_treat_prev),
                AD_unavoid=sum(AD_unavoid),
                total_deaths2=sum(total_deaths2),
                total_overall=sum(total_overall),
                py=sum(py),
                AD_sum=sum(AD_sum))%>%
  distinct()%>%
  as.data.frame()

AD_projections<-AD_projections%>%
  full_join(AD_projections_overall)


# ---------------------------------------------------------------------------
# Overall (all ages) and by age_cat, per year
# ---------------------------------------------------------------------------
AD_projections_overall <- AD_projections%>%
  dplyr::mutate(age_cat="Overall")%>%
  dplyr::group_by(year, country_code, cancer_code, sex, age_cat)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_sum.asr=sum(AD_sum/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  select( -total_overall)%>%
  dplyr::mutate(AD_prev = sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat = sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_treat_prev = sum(AD_treat_prev,na.rm=T))%>%
  dplyr::mutate(AD_sum = sum(AD_sum,na.rm=T))%>%
  dplyr::mutate(AD_unavoid = sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2,na.rm=T))%>%
  dplyr::mutate(py=sum(py))%>%
  select(-age)%>%
  ungroup()%>%
  dplyr::group_by(year, cancer_code, country_code, sex, age_cat)%>%
  dplyr::mutate(w=sum(w/2))%>%
  distinct()%>%
  as.data.frame()


AD_projections_age_cat <- AD_projections%>%
  ungroup()%>%
  dplyr::group_by(year, country_code, cancer_code, sex, age_cat)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_sum.asr=sum(AD_sum/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  select(-total_overall)%>%
  dplyr::mutate(AD_prev = sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat = sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_treat_prev = sum(AD_treat_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid = sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2,na.rm=T))%>%
  dplyr::mutate(AD_sum = sum(AD_sum,na.rm=T))%>%
  dplyr::mutate(py=sum(py))%>%
  select( -age)%>%
  ungroup()%>%
  dplyr::group_by(year, cancer_code, country_code, sex, age_cat)%>%
  dplyr::mutate(w=sum(w/2))%>%
  distinct()%>%
  full_join(AD_projections_overall)%>%
  as.data.frame()%>%
  ungroup()


# ---------------------------------------------------------------------------
# By HDI, per year
# ---------------------------------------------------------------------------
AD_by_HDI_year <- AD_projections%>%
  dplyr::mutate(age_cat="Overall", sex!=0)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  select(-country_label,-country_code,  hdi_group, cancer, cancer_code, AD_treat, AD_prev,
         AD_unavoid, AD_sum, -age, age_cat,-total_overall, year)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%
  dplyr::group_by(year, hdi_group, sex, cancer)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_prev+AD_treat))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2))%>%
  dplyr::mutate(AD_treat=sum(AD_treat))%>%
  dplyr::mutate(AD_prev=sum(AD_prev))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid))%>%
  dplyr::mutate(total_deaths=total_deaths2)%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  dplyr::mutate(w=sum(w)/n())%>%
  select(-AD_sum,-py)%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()


AD_by_HDI_all_year <-AD_projections%>%
  dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::group_by(year, hdi_group, cancer, age, sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  select(-country_label,-country_code,-cancer_code, -cancer,  hdi_group,  AD_treat, AD_prev,
         AD_unavoid, total_deaths2, AD_sum, -age, age_cat,-total_overall, year)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  dplyr::mutate(hdi_group=case_when(
    hdi_group==1~ 984, # low
    hdi_group==2~ 983, # medium
    hdi_group==3~ 982, # high
    hdi_group==4~ 981  # very-high
  ))%>%
  ungroup()%>%
  dplyr::group_by(year, hdi_group, sex)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
                AD_sum.asr=sum(AD_sum/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  dplyr::mutate(AD_treat_prev=sum(AD_prev+AD_treat))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2))%>%
  dplyr::mutate(AD_treat=sum(AD_treat))%>%
  dplyr::mutate(AD_prev=sum(AD_prev))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid))%>%
  dplyr::mutate(total_deaths=total_deaths2)%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  dplyr::mutate(cancer="All Cancer Sites")%>%
  dplyr::mutate(w=sum(w)/n())%>%
  select(-AD_sum,-py)%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()%>%
  dplyr::rename("country_code"="hdi_group")


# ---------------------------------------------------------------------------
# By country, per year
# ---------------------------------------------------------------------------
AD_country_year <-AD_projections%>%
  dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::group_by(year, country_label, cancer, age, sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  select(-age,-total_overall,-AD_sum)%>%
  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(-hdi_group)%>%
  dplyr::group_by(year, country_code, sex)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  dplyr::mutate(total_deaths=sum(AD_prev, AD_unavoid, AD_treat,na.rm=T))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2,na.rm=T))%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  select(-py,-w)%>%
  distinct()%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()


# ---------------------------------------------------------------------------
# World total, per year
# ---------------------------------------------------------------------------
AD_world_year <- AD_projections%>%
  dplyr::mutate(age_cat="Overall")%>%
  dplyr::group_by(year, cancer, age, sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(-hdi_group, -AD_sum, -age)%>%
  dplyr::mutate(country_code=900)%>%
  dplyr::mutate(country_label="All Countries")%>%
  dplyr::group_by(year, cancer, sex)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  select(-total_deaths2)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  select(-py,-w,-total_overall)%>%
  distinct()%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()


# ---------------------------------------------------------------------------
# By region, per year
# ---------------------------------------------------------------------------
AD_Region_year <- AD_projections%>%
  left_join(HDI_Region_Mapping2, by=c("country_code"))%>%
  dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::group_by(year, area, cancer, age, sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  select(-country_label,-country_code,-cancer_code, -cancer,  hdi_group,  AD_treat, AD_prev,
         AD_unavoid, total_deaths2, -age, age_cat,-total_overall, year)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(-hdi_group,-continent, -AD_sum)%>%
  dplyr::group_by(year, area, sex)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  select(-total_deaths2)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  dplyr::mutate(cancer="All Cancer Sites")%>%
  dplyr::mutate(cancer_code=40)%>%
  select(-py,-w)%>%
  distinct()%>%
  left_join(areas, by=c("area"))%>%
  ungroup()%>%
  distinct()%>%
  left_join(HDI_Region_Mapping, by=c("area", "country_label"))%>%
  as.data.frame()


# ---------------------------------------------------------------------------
# Table 1 equivalent, per year
# ---------------------------------------------------------------------------
table_1_1_year <- AD_projections %>%
  select(-country_label, -country_code, -cancer_code, hdi_group)%>%
  dplyr::mutate(age_cat="Overall")%>%
  dplyr::group_by(year, cancer, age, sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%
  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  group_by(year, sex)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w), #ASR calculation here
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  select("year", "sex", AD_prev, AD_treat,
         AD_treat_prev, AD_unavoid, total_deaths,
         pAD_treat,
         pAD_prev, pAD_treat_prev, pAD_unavoid,
         AD_prev.asr, AD_treat.asr,
         AD_treat_prev.asr, AD_unavoid.asr, total.deaths.asr)%>%
  distinct()


# ---------------------------------------------------------------------------
# Quick look: world trajectory
# ---------------------------------------------------------------------------
ataaa<-AD_world_year%>%
  filter(sex==0)%>%
  select(year, AD_prev, AD_treat, AD_treat_prev, total_deaths)%>%
  distinct()%>%
  arrange(year)



# bubble plot 

#Bubble plot -- 2024 vs 2050
library(dplyr)
library(ggplot2)
library(ggrepel)

# HDI value + group, one row per country
hdi_lookup <- HDI_2022  %>%
  ungroup() %>%
  dplyr::select(country_code, value, hdi_group) %>%
  distinct()%>%
  bind_rows(
    tibble(country_code = c(254, 258, 312, 474, 540, 638),   # French territories
           value = HDI_2022$value[HDI_2022$country_code == 250],
           hdi_group = HDI_2022$hdi_group[HDI_2022$country_code == 250]),
    tibble(country_code = c(316, 630),                        # US territories
           value = HDI_2022$value[HDI_2022$country_code == 840],
           hdi_group = HDI_2022$hdi_group[HDI_2022$country_code == 840])
  )
  
  

bubble_data <- AD_country_year %>%
  ungroup() %>%
  filter(year %in% c(2024, 2050),
         sex == 0,
         cancer_code == 7) %>%
  dplyr::select(country_code, country_label, year, AD = AD_treat_prev) %>%
  distinct() %>%
  left_join(hdi_lookup, by = "country_code") %>%
#  filter(AD > 0, !is.na(value)) %>%
  mutate(
    hdi_group = factor(hdi_group,
                       levels = c(1,2,3,4),
                       labels = c("Low", "Medium", "High", "Very High")),
    year_label = factor(year, levels = c(2024, 2050)))

# Flag top countries (top 10%) WITHIN each year
bubble_data <- bubble_data %>%
  group_by(year) %>%
  mutate(label_flag = AD >= quantile(AD, 0.9, na.rm = TRUE)) %>%
  ungroup()

bubble_plot <- bubble_data %>%
  ggplot(aes(x = value,
             y = AD,
             size = AD,
             colour = hdi_group)) +
  
  geom_jitter(alpha = 0.7, width = 0.15) +
  
  geom_text_repel(
    data = subset(bubble_data, label_flag),
    aes(label = country_label),
    size = 3.5,
    fontface = "bold",
    max.overlaps = Inf,
    box.padding = 0.4,
    point.padding = 0.3
  ) +
  
  facet_wrap(~ year_label, ncol = 1) +
  
  scale_size_continuous(range = c(2, 14)) +
  scale_y_log10(labels = scales::comma) +
  scale_colour_brewer(palette = "RdYlGn", direction = 1) +
  
  labs(

    x = "Human Development Index (HDI) group",
    y = "Avoidable deaths (log scale)",
    size = "Avoidable deaths",
    colour = "HDI Group"
  ) +
  
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank(),
    strip.text = element_text(face = "bold")
  )


bubble_plot




# HDI
hdi_shift <- AD_by_HDI_year %>%
  ungroup() %>%
  filter(year %in% c(2024, 2050), sex != 0, hdi_group %in% 1:4) %>%
  dplyr::group_by(year, hdi_group, sex) %>%
  dplyr::summarise(AD_treat_prev = first(AD_treat_prev),
                   AD_prev       = first(AD_prev),
                   AD_treat      = first(AD_treat),
                   total_deaths  = first(total_deaths),
                   .groups = "drop") %>%
  dplyr::group_by(year, hdi_group) %>%
  dplyr::summarise(across(c(AD_treat_prev, AD_prev, AD_treat, total_deaths), sum),
                   .groups = "drop") %>%
  mutate(hdi_group  = factor(hdi_group, levels = 1:4,
                             labels = c("Low", "Medium", "High", "Very High")),
         year_label = factor(year, levels = c(2024, 2050)))

nrow(hdi_shift)

# connector + percent change between the two years
hdi_segments <- hdi_shift %>%
  dplyr::select(hdi_group, year, AD_treat_prev) %>%
  pivot_wider(names_from = year, values_from = AD_treat_prev, names_prefix = "y") %>%
  mutate(pct_change = (y2050 / y2024 - 1) * 100,
         label      = sprintf("%+.0f%%", pct_change),
         label_y    = pmax(y2024, y2050))

hdi_bubble_plot <- ggplot(hdi_shift, aes(x = hdi_group, y = AD_treat_prev)) +
  
  geom_segment(data = hdi_segments,
               aes(x = hdi_group, xend = hdi_group, y = y2024, yend = y2050),
               inherit.aes = FALSE,
               colour = "grey70", linewidth = 1) +
  
  geom_point(aes(size = AD_treat_prev, colour = year_label), alpha = 0.85) +
  
  geom_text(data = hdi_segments,
            aes(x = hdi_group, y = label_y, label = label),
            inherit.aes = FALSE,
            vjust = -2.2, fontface = "bold", size = 4) +
  
  scale_size_continuous(range = c(4, 20), guide = "none") +
#  scale_colour_manual(values = c("2024" = "#a6cee3", "2050" = "#1f78b4")) +
  scale_y_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0.1, 0.25))) +
  
  labs(
   # title = "Avoidable gastric cancer deaths by HDI group",
    #subtitle = "GLOBOCAN 2024 and projected 2050, with percent change",
    x = "Human Development Index group",
    y = "Avoidable deaths",
    colour = NULL
  ) +
  
  theme_minimal(base_size = 13) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank())


hdi_bubble_plot

# saving the outputs



ggsave("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024\\Bubble_plot_countries.png",
       plot = bubble_plot, width = 15, height = 15, dpi = 500)

ggsave("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024\\Bubble_plot_HDI.png",
       plot = hdi_bubble_plot, width = 25, height = 40, dpi = 500)


# combined plot
# ---- helper: both-sexes total without touching the sex-0 rows -------------
both_sexes <- function(df, ...) {
  df %>%
    ungroup() %>%
    filter(year %in% c(2024, 2050), sex != 0) %>%
    dplyr::group_by(year, sex, ...) %>%
    dplyr::summarise(AD_treat_prev = first(AD_treat_prev), .groups = "drop") %>%
    dplyr::group_by(year, ...) %>%
    dplyr::summarise(AD_treat_prev = sum(AD_treat_prev), .groups = "drop")
}

# ---- regions -------------------------------------------------------------
region_shift <- AD_Region_year %>%
  both_sexes(country_label) %>%
  rename(group = country_label) %>%
  mutate(block = "Region")

# ---- HDI -----------------------------------------------------------------
hdi_shift <- AD_by_HDI_year %>%
  filter(hdi_group %in% 1:4) %>%
  both_sexes(hdi_group) %>%
  mutate(group = as.character(factor(hdi_group, levels = 1:4,
                                     labels = c("Low", "Medium", "High", "Very High"))),
         block = "HDI") %>%
  dplyr::select(-hdi_group)

# ---- world ---------------------------------------------------------------
world_shift <- AD_world_year %>%
  both_sexes() %>%
  mutate(group = "World", block = "Global")

# ---- reconcile before plotting -------------------------------------------
# regions and HDI must each sum to the world total, per year
bind_rows(region_shift, hdi_shift, world_shift) %>%
  group_by(year, block) %>%
  summarise(total = sum(AD_treat_prev), .groups = "drop") %>%
  pivot_wider(names_from = block, values_from = total)

# ---- stack, ordered: regions (by 2050), then HDI, then World -------------
combined <- bind_rows(region_shift, hdi_shift, world_shift) %>%
  mutate(year_label = factor(year, levels = c(2024, 2050)))

region_order <- combined %>%
  filter(block == "Region", year == 2050) %>%
  arrange(AD_treat_prev) %>% pull(group)

lvl <- c(region_order, "Low", "Medium", "High", "Very High", "World")

combined <- combined %>% mutate(group = factor(group, levels = lvl))



# ---- palette -------------------------------------------------------------
PAL <- c(
  ink   = "#1D3040",   # deep navy — text, rules
  teal  = "#2E6E6B",   # HDI band
  ochre = "#E4B363",   # 2024
  rust  = "#C1502E",   # 2050
  bone  = "#EDE6DB"    # Region band
)

# ---- block backgrounds and labels ----------------------------------------
n_reg   <- length(region_order)
X_LIMIT <- 800000

div1 <- n_reg + 0.5
div2 <- n_reg + 4.5

segs <- combined %>%
  dplyr::select(group, year, AD_treat_prev) %>%
  pivot_wider(names_from = year, values_from = AD_treat_prev, names_prefix = "y") %>%
  mutate(pct_change = (y2050 / y2024 - 1) * 100,
         label      = sprintf("%+.0f%%", pct_change),
         label_x    = pmin(pmax(y2024, y2050) * 1.02, X_LIMIT * 0.85))

blocks <- tibble(
  block = c("Region", "HDI", "Global"),
  ymin  = c(0.4,         n_reg + 0.5,   n_reg + 4.5),
  ymax  = c(n_reg + 0.5, n_reg + 4.5,   n_reg + 5.6),
  fill  = c(PAL[["bone"]], PAL[["teal"]], PAL[["ink"]]),
  alpha = c(0.60,          0.10,          0.14)
) %>%
  mutate(ymid = (ymin + ymax) / 2)

combined_plot <- ggplot(combined, aes(x = AD_treat_prev, y = group)) +
  
  geom_rect(data = blocks,
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax,
                fill = fill, alpha = alpha),
            inherit.aes = FALSE) +
  scale_fill_identity() +
  scale_alpha_identity() +
  
  geom_hline(yintercept = c(div1, div2),
             colour = PAL[["ink"]], linetype = "dashed", linewidth = 0.35) +
  
  geom_segment(data = segs,
               aes(x = y2024, xend = y2050, y = group, yend = group),
               inherit.aes = FALSE,
               colour = PAL[["ink"]], alpha = 0.30, linewidth = 0.9) +
  
  geom_point(aes(size = AD_treat_prev, colour = year_label),
             alpha = 0.9, stroke = 0) +
  scale_colour_manual(values = c("2024" = PAL[["ochre"]],
                                 "2050" = PAL[["rust"]])) +
  
  geom_text(data = segs,
            aes(x = label_x, y = group, label = label),
            inherit.aes = FALSE,
            hjust = -0.4, fontface = "bold", size = 3.4,
            colour = PAL[["ink"]]) +
  
  geom_text(data = blocks %>% filter(block != "Global"),
            aes(x = X_LIMIT * 0.97, y = ymid, label = block),
            inherit.aes = FALSE,
            angle = -90, fontface = "bold", size = 4.2,
            colour = PAL[["ink"]], alpha = 0.7) +
  
  scale_size_continuous(range = c(3, 16), guide = "none") +
  scale_x_continuous(labels = scales::comma,
                     limits = c(0, X_LIMIT),
                     expand = expansion(mult = c(0.02, 0.02))) +
  
  labs(x = "Avoidable deaths", y = NULL, colour = NULL) +
  
  theme_minimal(base_size = 13) +
  theme(legend.position   = "top",
        legend.text       = element_text(colour = PAL[["ink"]]),
        axis.text         = element_text(colour = PAL[["ink"]]),
        axis.title.x      = element_text(colour = PAL[["ink"]],
                                         margin = margin(t = 10)),
        panel.grid.minor  = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey88", linewidth = 0.3))

combined_plot

# saving the outputs


# saving the outputs



ggsave("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024\\Bubble_plot_countries.png",
       plot = bubble_plot, width = 15, height = 15, dpi = 500)

ggsave("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024\\Bubble_plot_HDI.png",
       plot = hdi_bubble_plot, width = 25, height = 40, dpi = 500)

ggsave("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024\\Bubble_plot_HDI_region.png",
       plot = combined_plot, width = 12, height = 9, dpi = 300)


