
NS_OS_PAF <- NS_OS %>%
  #filter(age_cat!="Overall")%>%
  left_join(PAFs2,
            by = c(
              "country_code",
              "cancer_code" = "cancer_code",
              "age_cat" = "age_cat"
            )) %>%
  left_join(ES2 %>% filter(sex == 2, year == 2012),
            by = c("country_code", "age_cat")) %>%
  droplevels() %>%
  mutate(cancer = as.character(cancer)) %>%
  distinct() %>%
  filter(Five_Year_all_cause_Surv <= 1) %>% #filtering out values for which the algorithm failed
  filter(Five_Year_Net_Surv <= 1) %>%
  mutate(surv_ref = 0.93) %>%
  left_join(HDI %>% select(country_code, hdi_group), by = c("country_code")) %>%
  ungroup() %>%
  group_by(age_cat, hdi_group) %>%
  mutate(median_ref = median(Five_Year_Net_Surv)) %>%
  mutate(max_ref = max(Five_Year_Net_Surv)) %>%
  rename("country_label" = "country") %>%
  ungroup() %>%
  select(-hdi_group)
#Rutherford AD
Avoidable_Deaths <- NS_OS_PAF %>%
  mutate(
    AD = (surv_ref - Five_Year_Net_Surv) * ES  * total_overall,
    AD_Lower = (surv_ref - NS_Upper_CI) *   ES * total_overall,
    AD_Upper = (surv_ref - NS_Lower_CI) *  ES * total_overall,
    AD_unavoid = total_overall * (1 - surv_ref * ES),
    total_deaths = AD + AD_unavoid,
    #Median in each HDI group as reference
    AD_med = (median_ref - Five_Year_Net_Surv) * ES  * total_overall,
    AD_Lower_med = (median_ref - NS_Upper_CI) *   ES * total_overall,
    AD_Upper_med = (median_ref - NS_Lower_CI) *  ES * total_overall,
    AD_unavoid_med = total_overall * (1 - median_ref * ES),
    #max in each HDI group as reference
    AD_max = (max_ref - Five_Year_Net_Surv) * ES  * total_overall,
    AD_Lower_max = (max_ref - NS_Upper_CI) *   ES * total_overall,
    AD_Upper_max = (max_ref - NS_Lower_CI) *  ES * total_overall,
    AD_unavoid_max = total_overall * (1 - max_ref * ES),
    #accounting for cases where the net survival is equal (or higher) than the references
    AD  = case_when(
      Five_Year_Net_Surv >= surv_ref ~ 0,
      Five_Year_Net_Surv < surv_ref ~ AD
    ),
    AD_Lower  = case_when(
      Five_Year_Net_Surv >= surv_ref ~ 0,
      Five_Year_Net_Surv < surv_ref ~ AD_Lower
    ),
    AD_Upper  = case_when(
      Five_Year_Net_Surv >= surv_ref ~ 0,
      Five_Year_Net_Surv < surv_ref ~ AD_Upper
    ),
    AD_med = case_when(
      Five_Year_Net_Surv >= median_ref ~ 0,
      Five_Year_Net_Surv < median_ref ~ AD_med
    ),
    AD_Lower_med = case_when(
      Five_Year_Net_Surv >= median_ref ~ 0,
      Five_Year_Net_Surv < median_ref ~ AD_Lower_med
    ),
    AD_Upper_med = case_when(
      Five_Year_Net_Surv >= median_ref ~ 0,
      Five_Year_Net_Surv < median_ref ~ AD_Upper_med
    ),
    AD_max = case_when(
      Five_Year_Net_Surv == max_ref ~ 0,
      Five_Year_Net_Surv != max_ref ~ AD_max
    ),
    AD_Lower_max = case_when(
      Five_Year_Net_Surv == max_ref ~ 0,
      Five_Year_Net_Surv != max_ref ~ AD_Lower_max
    ),
    AD_Upper_max = case_when(
      Five_Year_Net_Surv == max_ref ~ 0,
      Five_Year_Net_Surv != max_ref ~ AD_Upper_max
    )
  ) %>%
  #fixing any negative values so they aren't in the total
  select(
    "country_code",
    "country_label",
    "age_cat",
    "cancer_code",
    "cancer_label",
    "AD",
    "AD_Lower",
    "AD_Upper",
    "AD_med",
    "AD_Lower_med",
    "AD_Upper_med",
    "AD_max",
    "AD_Lower_max",
    "AD_Upper_max",
    "total_deaths",
    "total_overall"
  )
#Keeping subgroups for which both aren't available seperate from the overall group (to filter countries from the overall)
AD_countries_low <-
  Avoidable_Deaths %>% filter(age_cat == "15-64") %>% select(country_label)
AD_countries_upp <-
  Avoidable_Deaths %>% filter(age_cat == "65-99") %>% select(country_label)
AD_countries_overall <-
  Avoidable_Deaths %>% filter(age_cat == "Overall") %>% select(country_label)
no_overall_upp <-
  AD_countries_low %>% filter(!country_label %in% AD_countries_upp$country_label)
no_overall_low <-
  AD_countries_upp %>% filter(!country_label %in% AD_countries_low$country_label)
no_overall_o1 <-
  AD_countries_overall %>% filter(!country_label %in% AD_countries_low$country_label)
no_overall_o2 <-
  AD_countries_overall %>% filter(!country_label %in% AD_countries_upp$country_label)
Avoidable_Deaths_overall <- Avoidable_Deaths %>%
  as.data.frame() %>%
  filter(age_cat != "Overall") %>%
  filter(!country_label %in% no_overall_upp$country_label) %>%
  filter(!country_label %in% no_overall_low$country_label) %>%
  mutate(age_cat = "Overall") %>%
  ungroup() %>%
  group_by(country_code, cancer_code, age_cat) %>%
  dplyr::summarise(
    country_code,
    country_label,
    cancer_code,
    cancer_label,
    total_overall = sum(total_overall),
    AD = sum(AD),
    AD_Lower = sum(AD_Lower),
    AD_Upper = sum(AD_Upper),
    AD_med = sum(AD_med),
    AD_Lower_med = sum(AD_Lower_med),
    AD_Upper_med = sum(AD_Upper_med),
    AD_max = sum(AD_max),
    AD_Lower_max = sum(AD_Lower_max),
    AD_Upper_max = sum(AD_Upper_max),
    total_deaths = sum(total_deaths)
  ) %>%
  distinct() %>%
  as.data.frame()
Avoidable_Deaths_overall2 <- Avoidable_Deaths %>%
  as.data.frame() %>%
  filter(country_label %in% no_overall_o2$country_label) %>%
  filter(age_cat == "Overall") %>%
  distinct() %>%
  as.data.frame()
Avoidable_Deaths_age_cat <- Avoidable_Deaths %>%
  group_by(country_code, cancer_code, age_cat) %>%
  mutate(AD = sum(AD)) %>%
  full_join(Avoidable_Deaths_overall) %>%
  full_join(Avoidable_Deaths_overall2) %>%
  dplyr::mutate(pAD = AD / total_deaths) %>%
  dplyr::mutate(pAD_Lower = AD_Lower / total_deaths) %>%
  dplyr::mutate(pAD_Upper = AD_Upper / total_deaths) %>%
  dplyr::mutate(pAD_med = AD_med / total_deaths) %>%
  dplyr::mutate(pAD_Lower_med = AD_Lower_med / total_deaths) %>%
  dplyr::mutate(pAD_Upper_med = AD_Upper_med / total_deaths) %>%
  dplyr::mutate(pAD_max = AD_max / total_deaths) %>%
  dplyr::mutate(pAD_Lower_max = AD_Lower_max / total_deaths) %>%
  dplyr::mutate(pAD_Upper_max = AD_Upper_max / total_deaths) %>%
  distinct() %>%
  as.data.frame()
HDI
AD_HDI <- Avoidable_Deaths_age_cat %>%
  left_join(HDI %>% select(country_code, hdi_group), by = c("country_code")) %>%
  select(-country_code) %>%
  group_by(hdi_group, age_cat) %>%
  dplyr::summarise(
    hdi_group,
    cancer_code,
    cancer_label,
    total_overall = sum(total_overall),
    AD = sum(AD),
    AD_Lower = sum(AD_Lower),
    AD_Upper = sum(AD_Upper),
    AD_med = sum(AD_med),
    AD_Lower_med = sum(AD_Lower_med),
    AD_Upper_med = sum(AD_Upper_med),
    AD_max = sum(AD_max),
    AD_Lower_max = sum(AD_Lower_max),
    AD_Upper_max = sum(AD_Upper_max),
    total_deaths = sum(total_deaths)
  ) %>%
  distinct() %>%
  dplyr::mutate(pAD = AD / total_deaths) %>%
  dplyr::mutate(pAD_Lower = AD_Lower / total_deaths) %>%
  dplyr::mutate(pAD_Upper = AD_Upper / total_deaths) %>%
  dplyr::mutate(pAD_med = AD_med / total_deaths) %>%
  dplyr::mutate(pAD_Lower_med = AD_Lower_med / total_deaths) %>%
  dplyr::mutate(pAD_Upper_med = AD_Upper_med / total_deaths) %>%
  dplyr::mutate(pAD_max = AD_max / total_deaths) %>%
  dplyr::mutate(pAD_Lower_max = AD_Lower_max / total_deaths) %>%
  dplyr::mutate(pAD_Upper_max = AD_Upper_max / total_deaths) %>%
  distinct() %>%
  as.data.frame()
# Calculating by region. Need a file that links countries to region
region_Seychelles2 <- data.frame(c(690), c(1), c(1), c("Seychelles"))
colnames(region_Seychelles2) <-
  c("country_code", "continent", "area", "country_label")
HDI_Region_Mapping2 <- HDI_Region_Mapping %>%
  full_join(region_Seychelles2) %>%
  select(-country_label) %>%
  dplyr::filter(area <= 21)
areas <- HDI_Region_Mapping %>%
  dplyr::filter(
    country_code >= 910 & country_code <= 931 |
      country_code == 905 | country_code == 906 |
      country_code == 954 | country_code == 957
  ) %>%
  select(area, country_label)
AD_region <- Avoidable_Deaths_age_cat %>%
  select(-country_label) %>%
  left_join(HDI_Region_Mapping2) %>%
  left_join(areas) %>%
  select(-country_code) %>%
  group_by(area, age_cat) %>%
  dplyr::summarise(
    area,
    country_label,
    cancer_code,
    cancer_label,
    total_overall = sum(total_overall),
    AD = sum(AD),
    AD_Lower = sum(AD_Lower),
    AD_Upper = sum(AD_Upper),
    AD_med = sum(AD_med),
    AD_Lower_med = sum(AD_Lower_med),
    AD_Upper_med = sum(AD_Upper_med),
    AD_max = sum(AD_max),
    AD_Lower_max = sum(AD_Lower_max),
    AD_Upper_max = sum(AD_Upper_max),
    total_deaths = sum(total_deaths)
  ) %>%
  distinct() %>%
  dplyr::mutate(pAD = AD / total_deaths) %>%
  dplyr::mutate(pAD_Lower = AD_Lower / total_deaths) %>%
  dplyr::mutate(pAD_Upper = AD_Upper / total_deaths) %>%
  dplyr::mutate(pAD_med = AD_med / total_deaths) %>%
  dplyr::mutate(pAD_Lower_med = AD_Lower_med / total_deaths) %>%
  dplyr::mutate(pAD_Upper_med = AD_Upper_med / total_deaths) %>%
  dplyr::mutate(pAD_max = AD_max / total_deaths) %>%
  dplyr::mutate(pAD_Lower_max = AD_Lower_max / total_deaths) %>%
  dplyr::mutate(pAD_Upper_max = AD_Upper_max / total_deaths) %>%
  distinct() %>%
  as.data.frame()
#Summing by country, region, and such
AD_all <-
  Avoidable_Deaths_age_cat %>% #Missing countries for which no overall was calculated. Here the overall will be calculated by running the overall survival
  as.data.frame() %>%
  mutate(country_code = 1000) %>%
  mutate(country_label = "All countries") %>%
  group_by(age_cat) %>%
  mutate(AD = sum(AD, na.rm = TRUE)) %>%
  mutate(AD_Lower = sum(AD_Lower, na.rm = TRUE)) %>%
  mutate(AD_Upper = sum(AD_Upper, na.rm = TRUE)) %>%
  mutate(AD_med = sum(AD_med, na.rm = TRUE)) %>%
  mutate(AD_Lower_med = sum(AD_Lower_med, na.rm = TRUE)) %>%
  mutate(AD_Upper_med = sum(AD_Upper_med, na.rm = TRUE)) %>%
  mutate(AD_max = sum(AD_max, na.rm = TRUE)) %>%
  mutate(AD_Lower_max = sum(AD_Lower_max, na.rm = TRUE)) %>%
  mutate(AD_Upper_max = sum(AD_Upper_max, na.rm = TRUE)) %>%
  mutate(total_overall = sum(total_overall, na.rm = TRUE)) %>%
  mutate(total_deaths = sum(total_deaths, na.rm = TRUE)) %>%
  dplyr::mutate(pAD = AD / total_deaths) %>%
  dplyr::mutate(pAD_Lower = AD_Lower / total_deaths) %>%
  dplyr::mutate(pAD_Upper = AD_Upper / total_deaths) %>%
  dplyr::mutate(pAD_med = AD_med / total_deaths) %>%
  dplyr::mutate(pAD_Lower_med = AD_Lower_med / total_deaths) %>%
  dplyr::mutate(pAD_Upper_med = AD_Upper_med / total_deaths) %>%
  dplyr::mutate(pAD_max = AD_max / total_deaths) %>%
  dplyr::mutate(pAD_Lower_max = AD_Lower_max / total_deaths) %>%
  dplyr::mutate(pAD_Upper_max = AD_Upper_max / total_deaths) %>%
  ungroup() %>%
  distinct() %>%
  as.data.frame()
Avoidable_Deaths_age_cat2 <- Avoidable_Deaths_age_cat %>%
  dplyr::mutate(across(6:15, round,-1)) %>%
  dplyr::mutate(across(17:25, round, 3) * 100) %>% #dplyr::mutate to show proportion as percentage in export
  select(
    "country_code",
    "country_label",
    "age_cat",
    "cancer_code",
    "cancer_label",
    "AD",
    "AD_Lower",
    "AD_Upper",
    "pAD",
    "pAD_Lower",
    "pAD_Upper",
    "AD_med",
    "AD_Lower_med",
    "AD_Upper_med",
    "pAD_med",
    "pAD_Lower_med",
    "pAD_Upper_med",
    "AD_max",
    "AD_Lower_max",
    "AD_Upper_max",
    "pAD_max",
    "pAD_Lower_max",
    "pAD_Upper_max",
    "total_deaths"
  ) %>%
  arrange(age_cat, country_label)
AD_HDI2 <- AD_HDI %>%
  rename("country_label" = "hdi_group") %>%
  dplyr::mutate(country_label = as.character(country_label)) %>%
  select(
    "country_label",
    "age_cat",
    "cancer_code",
    "cancer_label",
    "AD",
    "AD_Lower",
    "AD_Upper",
    "AD_med",
    "AD_Lower_med",
    "AD_Upper_med",
    "AD_max",
    "AD_Lower_max",
    "AD_Upper_max",
    "total_deaths",
    "pAD",
    "pAD_Lower",
    "pAD_Upper",
    "pAD_med",
    "pAD_Lower_med",
    "pAD_Upper_med",
    "pAD_max",
    "pAD_Lower_max",
    "pAD_Upper_max"
  ) %>%
  dplyr::mutate(across(5:14, round,-1)) %>%
  dplyr::mutate(across(15:23, round, 3) * 100) %>% #dplyr::mutate to show proportion as percentage in export
  select(
    "country_label",
    "age_cat",
    "cancer_code",
    "cancer_label",
    "AD",
    "AD_Lower",
    "AD_Upper",
    "pAD",
    "pAD_Lower",
    "pAD_Upper",
    "AD_med",
    "AD_Lower_med",
    "AD_Upper_med",
    "pAD_med",
    "pAD_Lower_med",
    "pAD_Upper_med",
    "AD_max",
    "AD_Lower_max",
    "AD_Upper_max",
    "pAD_max",
    "pAD_Lower_max",
    "pAD_Upper_max",
    "total_deaths"
  ) %>%
  arrange(age_cat, country_label)
AD_region2 <- AD_region %>%
  select(
    "area",
    "country_label",
    "age_cat",
    "cancer_code",
    "cancer_label",
    "AD",
    "AD_Lower",
    "AD_Upper",
    "AD_med",
    "AD_Lower_med",
    "AD_Upper_med",
    "AD_max",
    "AD_Lower_max",
    "AD_Upper_max",
    "total_deaths",
    "pAD",
    "pAD_Lower",
    "pAD_Upper",
    "pAD_med",
    "pAD_Lower_med",
    "pAD_Upper_med",
    "pAD_max",
    "pAD_Lower_max",
    "pAD_Upper_max"
  ) %>%
  dplyr::mutate(across(6:15, round,-1)) %>%
  dplyr::mutate(across(16:24, round, 3) * 100) %>% #dplyr::mutate to show proportion as percentage in export
  select(
    "area",
    "country_label",
    "age_cat",
    "cancer_code",
    "cancer_label",
    "AD",
    "AD_Lower",
    "AD_Upper",
    "pAD",
    "pAD_Lower",
    "pAD_Upper",
    "AD_med",
    "AD_Lower_med",
    "AD_Upper_med",
    "pAD_med",
    "pAD_Lower_med",
    "pAD_Upper_med",
    "AD_max",
    "AD_Lower_max",
    "AD_Upper_max",
    "pAD_max",
    "pAD_Lower_max",
    "pAD_Upper_max",
    "total_deaths"
  ) %>%
  arrange(age_cat, area)
AD_all2 <- AD_all %>%
  dplyr::mutate(across(6:15, round,-1)) %>%
  dplyr::mutate(across(17:25, round, 3) * 100) %>% #dplyr::mutate to show proportion as percentage in export
  select(
    "country_code",
    "country_label",
    "age_cat",
    "cancer_code",
    "cancer_label",
    "AD",
    "AD_Lower",
    "AD_Upper",
    "pAD",
    "pAD_Lower",
    "pAD_Upper",
    "AD_med",
    "AD_Lower_med",
    "AD_Upper_med",
    "pAD_med",
    "pAD_Lower_med",
    "pAD_Upper_med",
    "AD",
    "AD_Lower_max",
    "AD_Upper_max",
    "AD_max",
    "pAD_max",
    "pAD_Lower_max",
    "pAD_Upper_max",
    "total_deaths"
  ) %>%
  arrange(age_cat, country_label)
NS_OS_PAF
AD_table_main <- AD_region2 %>%
  full_join(AD_HDI2) %>%
  full_join(AD_all2) %>%
  mutate(
    "Number 1" = paste0(AD, " (", AD_Lower, ", ", AD_Upper, ")"),
    "Proportion 1 (%)" = paste0(pAD, " (", pAD_Lower, ", ", pAD_Upper, ")"),
    "Number 2" = paste0(AD_med, " (", AD_Lower_med, ", ", AD_Upper_med, ")"),
    "Proportion 2 (%)" = paste0(pAD_med, " (", pAD_Lower_med, ", ", pAD_Upper_med, ")"),
    "Number 3" = paste0(AD_max, " (", AD_Lower_max, ", ", AD_Upper_max, ")"),
    "Proportion 3 (%)" = paste0(pAD_max, " (", pAD_Lower_max, ", ", pAD_Upper_max, ")")
  ) %>%
  rename("Country" = "country_label") %>%
  rename("Age Group" = "age_cat") %>%
  select(
    "Country",
    "Age Group",
    "Number 1",
    "Proportion 1 (%)",
    "Number 2",
    "Proportion 2 (%)",
    "Number 3",
    "Proportion 3 (%)"
  )
AD_table_countries <- Avoidable_Deaths_age_cat2 %>%
  mutate(
    "Number 1" = paste0(AD, " (", AD_Lower, ", ", AD_Upper, ")"),
    "Proportion 1 (%)" = paste0(pAD, " (", pAD_Lower, ", ", pAD_Upper, ")"),
    "Number 2" = paste0(AD_med, " (", AD_Lower_med, ", ", AD_Upper_med, ")"),
    "Proportion 2 (%)" = paste0(pAD_med, " (", pAD_Lower_med, ", ", pAD_Upper_med, ")"),
    "Number 3" = paste0(AD_max, " (", AD_Lower_max, ", ", AD_Upper_max, ")"),
    "Proportion 3 (%)" = paste0(pAD_max, " (", pAD_Lower_max, ", ", pAD_Upper_max, ")")
  ) %>%
  rename("Country" = "country_label") %>%
  rename("Age Group" = "age_cat") %>%
  select(
    "Country",
    "Age Group",
    "Number 1",
    "Proportion 1 (%)",
    "Number 2",
    "Proportion 2 (%)",
    "Number 3",
    "Proportion 3 (%)"
  )
check_ncountries <- Avoidable_Deaths_age_cat %>%
  group_by(age_cat) %>%
  mutate(n_countries = n()) %>%
  select(age_cat, n_countries) %>%
  distinct()
check_ncountries
#Checking number of countries in each HDI group
AD_HDI_n <- Avoidable_Deaths_age_cat %>%
  left_join(HDI %>% select(country_code, hdi_group), by = c("country_code")) %>%
  group_by(age_cat, hdi_group) %>%
  select(age_cat, hdi_group, country_code, country_label) %>%
  mutate(n_countries = n())
#Checking number of countries in each subregion
AD_region_n <- Avoidable_Deaths_age_cat %>%
  select(-country_label) %>%
  left_join(HDI_Region_Mapping2) %>%
  left_join(areas) %>%
  select(-country_code) %>%
  group_by(age_cat, area) %>%
  select(age_cat, area, country_label) %>%
  mutate(n_countries = n()) %>%
  distinct()
#Checking number of countries in each region - seems more reasonable to group by region...
AD_continent_n <- Avoidable_Deaths_age_cat %>%
  select(-country_label) %>%
  left_join(HDI_Region_Mapping2) %>%
  # left_join(areas)%>%
  select(-country_code) %>%
  group_by(age_cat, continent) %>%
  select(age_cat, continent) %>%
  mutate(n_countries = n()) %>%
  distinct()
write.csv2(
  AD_table_main,
  "\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\Breast Cancer\\Results\\table_main_Survcan.csv",
  row.names = F
)
write.csv2(
  AD_table_countries,
  "\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\Breast Cancer\\Results\\table_countries_Survcan.csv",
  row.names = F
)

#Checking number of countries in each region - seems more reasonable to group by region as we avoid regions represented by one country...
AD_continent_n <- Avoidable_Deaths_age_cat %>%
  select(-country_label) %>%
  left_join(HDI_Region_Mapping2) %>%
  # left_join(areas)%>%
  select(-country_code) %>%
  group_by(age_cat, continent) %>%
  select(age_cat, continent) %>%
  mutate(n_countries = n()) %>%
  distinct()

#Checking number of countries in each region - seems more reasonable to group by region as we avoid regions represented by one country...
AD_continent_n <- Avoidable_Deaths_age_cat %>%
  select(-country_label) %>%
  left_join(HDI_Region_Mapping2) %>%
  # left_join(areas)%>%
  select(-country_code) %>%
  group_by(age_cat, continent) %>%
  select(age_cat, continent) %>%
  distinct() %>%
  mutate(n_countries = n())

#Checking number of countries in each region - seems more reasonable to group by region as we avoid regions represented by one country...
AD_continent_n <- Avoidable_Deaths_age_cat %>%
  select(-country_label) %>%
  left_join(HDI_Region_Mapping2, by = c("country_code")) %>%
  # left_join(areas)%>%
  select(-country_code) %>%
  group_by(age_cat, continent) %>%
  select(age_cat, continent) %>%
  mutate(n_countries = n())
#Checking number of countries in each region - seems more reasonable to group by region as we avoid regions represented by one country...
AD_continent_n <- Avoidable_Deaths_age_cat %>%
  select(-country_label) %>%
  left_join(HDI_Region_Mapping2, by = c("country_code")) %>%
  # left_join(areas)%>%
  select(-country_code) %>%
  group_by(age_cat, continent) %>%
  select(age_cat, continent) %>%
  mutate(n_countries = n()) %>%
  distinct()
#Checking number of countries in each region - seems more reasonable to group by region as we avoid regions represented by one country...
AD_continent_n <- Avoidable_Deaths_age_cat %>%
  # select(-country_label)%>%
  left_join(HDI_Region_Mapping2, by = c("country_code")) %>%
  # left_join(areas)%>%
  select(-country_code) %>%
  group_by(age_cat, continent) %>%
  select(age_cat, continent) %>%
  mutate(n_countries = n()) %>%
  distinct()
View(AD_continent_n)
#Checking number of countries in each region - seems more reasonable to group by region as we avoid regions represented by one country...
AD_continent_n <- Avoidable_Deaths_age_cat %>%
  # select(-country_label)%>%
  left_join(HDI_Region_Mapping2, by = c("country_code")) %>%
  # left_join(areas)%>%
  select(-country_code) %>%
  group_by(age_cat, continent) %>%
  select(country_label, age_cat, continent) %>%
  mutate(n_countries = n()) %>%
  distinct()