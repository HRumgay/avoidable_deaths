  
  ES2<-p%>%as.data.frame()
  
  ten_cancer_sites <-
    Cancer_codes %>% 
    filter(cancer_code %in% c(20))#%>%6, 7, 11, 13, 15, 20, 23, 27, 30, 38
  # mutate(cancer_label=replace(cancer_label,cancer_label=="Unspecified sites","Colorectal"))
  
  
  #Prepping cancer real world data
  bcan_SURV2 <- bcan_SURV %>%
    distinct()%>%
    filter(surv_ddtot>0)%>%
    filter(include == "Included") %>%
    filter(age >= 15, age <= 99) %>%
    filter(!is.na(age), !is.na(surv_ddtot)) %>%
    mutate(age_cat = cut(age,
                         breaks = c(-Inf, 14, 49 , 99),
                         labels = c("<15", "15-49", "50-99")
    )) %>% #create age categories (to be adjusted)
    ungroup()%>%
    mutate(country=replace(country,country=="Cote d'Ivoire", "C?te d'Ivoire"),
           country=replace(country,country=="France", "Martinique"))%>%
    left_join(country_codes, by = c("country"="country_label"))%>%
    droplevels()%>%
    mutate(last_FU_age = round(age + surv_ddtot/365.25)) %>% #creating variable for age of death
    mutate(last_FU_year = round(year + surv_ddtot/365.25))%>%  #creating variable for year of death
    mutate(sex = replace(sex, sex == "Male", 1),
           sex = replace(sex, sex == "Female", 2),
           sex = as.integer(sex))%>% 
    left_join(life_table%>%filter(sex==2), by = c("last_FU_age" = "age",
                                                  "last_FU_year" = "year",
                                                  "country_code"="country_code")) %>%
    droplevels()%>%
    #filter(last_FU_year > 2008 & last_FU_year <= 2012) %>%
    #filter(!is.na(mx)) %>% 
    group_by(country_code)%>%
    dplyr::mutate(n=n())%>%
    ungroup()%>%
    #filter(n>=30)%>% #filter so at least 30 patients
    droplevels() %>%
    left_join(Cancer_codes_Survcan, by = c("cancer" = "cancer"))
  
  
  bcan_SURV2_overall<-bcan_SURV2%>%mutate(age_cat="Overall")
  
  bcan_SURV3 <- bcan_SURV2%>%
    full_join(bcan_SURV2_overall)%>%
    mutate(surv_yydd=surv_ddtot/365.25)%>%
    mutate(event1=case_when(dead==1 & surv_yydd<=5 ~ 1, #censoring survival after five years
                            dead==1 & surv_yydd>5 ~ 0,
                            dead==0 ~ 0
    ))%>%
    group_by(country_code, age_cat)%>%
    dplyr::mutate(n_events=sum(event1))%>%
    ungroup()%>%
    filter(n_events>=10)%>% #filter each subgroup so at least 10 events...
    ungroup()
  
  
  # a<-bcan_SURV2%>%select(country,country_code)%>%distinct()%>%as.data.frame()
  # b<-life_table%>%select(country,country_code)%>%distinct()%>%as.data.frame()
  
  
  
  #modifying so last five years of follow up

  #to check - it matches what is below
  
table_diagnosis<-  bcan_SURV3%>%  
    ungroup()%>%
    group_by(country, age_cat)%>%
    mutate(min_year=min(year),
           max_year=max(year),
           dif=max_year-min_year)%>%
    select(country, age_cat, min_year, max_year,dif)%>%
    distinct()
  
  bcan_SURV11 <- bcan_SURV3%>%
    as.data.frame()%>%
    select(region_lab, country, doi, last_FU_age,age, age_cat, surv_yytot,year,cancer_code)%>%
    group_by(region_lab, country, cancer_code, age_cat)%>%
    mutate(end_FU = max(year))%>%
    distinct()%>%
    as.data.frame()
  
  
  bSURV<-bcan_SURV3%>%
    left_join(bcan_SURV11)%>%
    ungroup()%>%
    group_by(country, age_cat)%>%
    ungroup()%>%
    filter(year>=end_FU-4)%>% #Filtering so last follow up was within a five year period
    group_by(country_code, age_cat)%>%
    mutate(max_fu=max(surv_yydd))%>%
    ungroup()%>%
    filter(max_fu>=5) #filtering so there is maximum survival time of at least five years...

  
  # 
  #  mutate(surv_yydd=case_when(surv_yydd<=5 ~ 5,
  #                           surv_yydd>5 ~ surv_yydd
  # )
  # )
  
  #bSURV$sex <- as.integer(bSURV$sex)
  
  #survcan_test<-bSURV%>%filter(cancer_code==30)
  
  
  #age categories
  #bSURV_overall <- bSURV %>% mutate(age_cat = "Overall")
  bSURV_Lower <- bSURV %>% filter(age_cat == "15-49")%>% ungroup()%>%  droplevels()
  bSURV_Upper <- bSURV %>% filter(age_cat == "50-99")%>% ungroup()%>%  droplevels()
  bSURV_Overall <- bSURV %>% filter(age_cat == "Overall")%>% ungroup()%>%  droplevels()
  
  
  
  
  missingmx <- bSURV %>%filter(is.na(mx)) #60 people have ages above 100 but were between 15-99 years at age of diagnosis. Should they be excluded?
  
  names(table(bcan_SURV2$country))
  
  #########################################
  #
  #
  ## Fitting a cubic spline by country and adding comparisons.
  #
  #
  #########################################
  
  #Creating time points for predictions
  time <- seq(0, 5, le = 5001)
  
  #Names for countries, regions and age groups for next step
  #Removes empty age variables for model to be run properly
  bSURV <- bSURV %>% 
    ungroup()%>%  
    droplevels()%>%
    arrange(country)
  
  #bSURV%>%select(country,bSURV$country_code)
  
  
  country_names_Survcan <- bSURV %>% select(country_code, country)%>%distinct()
  
  #   country_names_Survcan<- as_tibble(names(table(bSURV$country)))
  # names(country_names_Survcan)[names(country_names_Survcan) == "value"] <- "country"
  # country_names_Survcan <- country_names_Survcan%>%#For regression needs to be in this form
  # as.data.frame()%>%mutate(country=as.character(country))%>%slice(-c(4,13,19,22,31))
  
  country_codes2 <- as_tibble(names(table(bSURV$country_code))) #Needs to be tibble for predictions
  names(country_codes2)[names(country_codes2) == "value"] <- "country_code"
  country_codes2 <- as.data.frame(country_codes2)#%>%slice(-c(4,13,19,22,31)) #For regression needs to be in this form
  
  
  cancer_types <- names(table(bSURV$cancer))%>%as.data.frame() 
  names(cancer_types)[names(cancer_types) == "value"] <- "cancer"
  cancer_types <-  as.data.frame(cancer_types) #For regression needs to be in this form
  
  cancer_codes <- as_tibble(names(table(ten_cancer_sites$cancer_code))) #Needs to be tibble for predictions
  names(cancer_codes)[names(cancer_codes) == "value"] <- "cancer_code"
  cancer_codes <- as.data.frame(cancer_codes) #For regression needs to be in this form
  
  
  #Cubic base model BY COUNTRY
  
  
  Cubic_age_1 <- list()
  Cubic_age_2 <- list()
  Cubic_age_3 <- list()
  
  #Cubic base model BY Cancer type
  
  for (i in 1:nrow(country_codes2)) {
    b1 <- bSURV_Lower %>% filter(country_code == country_codes2[i,])
    b2 <- bSURV_Upper %>% filter(country_code == country_codes2[i,])
    b3 <- bSURV_Overall %>% filter(country_code == country_codes2[i,])
    
    
    
    k1 <- median(b1$surv_yydd)
    k2 <- median(b2$surv_yydd)
    k3 <- median(b3$surv_yydd)
    
    try(Cubic_age_1[[i]] <-
          mexhaz(
            formula = Surv(surv_yydd, event1) ~ 1,
            data = b1,
            base = "exp.ns",
            # degree = 3,
            knots = k1
            #  numHess=TRUE,
            # fnoptim="optim"
          ))
    
    try(Cubic_age_2[[i]] <-
          mexhaz(
            formula = Surv(surv_yydd, event1) ~ 1,
            data = b2,
            base = "exp.ns",
            # degree = 3,
            knots = k2
            # numHess=TRUE,
            # fnoptim="optim"
          ))
    
    try(Cubic_age_3[[i]] <-
          mexhaz(
            formula = Surv(surv_yydd, event1) ~ 1,
            data = b3,
            base = "exp.ns",
            # degree = 3,
            knots = k3
            # numHess=TRUE,
            # fnoptim="optim"
          ))
    
  }
  
  
  
  
  
  
  #Updating the models with mortality rates
  
  #Country updated models list initializing
  Cubic_Cancer_age_1 <- list()
  Cubic_Cancer_age_2 <- list()
  Cubic_Cancer_age_3 <- list()
  
  #Cubic excess hazard by country
  for (i in 1:nrow(country_codes2)){
    b1 <- bSURV_Lower %>% filter(country_code == country_codes2[i,])%>%as.data.frame()
    b2 <- bSURV_Upper %>% filter(country_code == country_codes2[i,])%>%as.data.frame()
    b3 <- bSURV_Overall %>% filter(country_code == country_codes2[i,])%>%as.data.frame()
    
    k1 <- median(b1$surv_yydd)
    k2 <- median(b2$surv_yydd)
    k3 <- median(b3$surv_yydd)
    
    try(Cubic_Cancer_age_1[[i]] <-
          update(Cubic_age_1[[i]], expected = "mx",      numHess=TRUE,
                 fnoptim="optim"))
    try(Cubic_Cancer_age_2[[i]] <-
          update(Cubic_age_2[[i]], expected = "mx",      numHess=TRUE,
                 fnoptim="optim"))
    try(Cubic_Cancer_age_3[[i]] <-
          update(Cubic_age_3[[i]], expected = "mx",      numHess=TRUE,
                 fnoptim="optim"))
    
  }
  
  
  
  ####################
  
  # Running Predictions by country
  
  ####################
  
  
  Predictions_Cubic_Net_age_1 <- list()
  Predictions_Cubic_Net_age_2 <- list()
  Predictions_Cubic_Net_age_3 <- list()
  
  Predictions_Cubic_All_Cause_age_1 <- list()
  Predictions_Cubic_All_Cause_age_2 <- list()
  Predictions_Cubic_All_Cause_age_3 <- list()
  
  
  country_codes2_tibble<-
    country_codes2 %>%
    mutate(country_code=as.integer(country_code))%>%
    as_tibble()
  
  cancer_types_tibble <-
    cancer_types %>% as_tibble() #predict only works with tibble data structure...
  
  cancer_codes_tibble <-
    cancer_codes %>% 
    mutate(cancer_code=as.integer(cancer_code))%>%
    as_tibble() #predict only works with tibble data structure...

  
#Cubic predictions by country
  
  for (i in 1:nrow(country_codes2_tibble)) {
    try(AC1 <- predict(Cubic_age_1[[i]], time.pts = time, data.val = country_codes2_tibble[i,]))
    try(AC2 <- predict(Cubic_age_2[[i]], time.pts = time, data.val = country_codes2_tibble[i,]))
    try(AC3 <- predict(Cubic_age_3[[i]], time.pts = time, data.val = country_codes2_tibble[i,]))  
    
    try(HP1 <- predict(Cubic_Cancer_age_1[[i]],
                       time.pts = time,
                       data.val = country_codes2_tibble[i,]))
    try(HP2 <- predict(Cubic_Cancer_age_2[[i]],
                       time.pts = time,
                       data.val = country_codes2_tibble[i,]))
    try(HP3 <- predict(Cubic_Cancer_age_3[[i]],
                       time.pts = time,
                       data.val = country_codes2_tibble[i,]))
    
    
    
    Predictions_Cubic_All_Cause_age_1[[i]] <- AC1
    Predictions_Cubic_All_Cause_age_2[[i]] <- AC2
    Predictions_Cubic_All_Cause_age_3[[i]] <- AC3
    
    
    Predictions_Cubic_Net_age_1[[i]] <- HP1
    Predictions_Cubic_Net_age_2[[i]] <- HP2
    Predictions_Cubic_Net_age_3[[i]] <- HP3
  }
  
  
  #Extracting prediction data five year avoidable deaths prediction and survival
  
  Net_Survival_Five_Year_age_1 <- matrix(ncol = 4, nrow = nrow(country_codes2)) #R(t)
  Net_Survival_Five_Year_age_2 <- matrix(ncol = 4, nrow = nrow(country_codes2)) #R(t)
  Net_Survival_Five_Year_age_3 <- matrix(ncol = 4, nrow = nrow(country_codes2)) #R(t)
  
  
  All_Cause_Survival_age_1 <- matrix(ncol = 4, nrow = nrow(country_codes2))    #S(t)
  All_Cause_Survival_age_2 <- matrix(ncol = 4, nrow = nrow(country_codes2) )    #S(t)
  All_Cause_Survival_age_3 <- matrix(ncol = 4, nrow = nrow(country_codes2) )    #S(t)
  
  for (i in 1:nrow(country_codes2_tibble)) {
    s <-  Predictions_Cubic_Net_age_1[[i]]$results
    s <-  s %>% filter(time.pts == 5)
    s2 <-  Predictions_Cubic_Net_age_2[[i]]$results
    s2 <-  s2 %>% filter(time.pts == 5)
    s3 <-  Predictions_Cubic_Net_age_3[[i]]$results
    s3 <-  s3 %>% filter(time.pts == 5)
    # 
    
    sp <- Predictions_Cubic_All_Cause_age_1[[i]]$results
    sp <-  sp %>% filter(time.pts == 5)
    sp2 <- Predictions_Cubic_All_Cause_age_2[[i]]$results
    sp2 <-  sp2 %>% filter(time.pts == 5)
    sp3 <- Predictions_Cubic_All_Cause_age_3[[i]]$results
    sp3 <-  sp3 %>% filter(time.pts == 5)
    
    try(Net_Survival_Five_Year_age_1[i,] <-
          c(country_codes2[i,],
            #country_names_Survcan[i,1], 
            s$surv, 
            s$surv.inf, 
            s$surv.sup))
    
    try(Net_Survival_Five_Year_age_2[i,] <-
          c(country_codes2[i,],
            #country_names_Survcan[i,1], 
            s2$surv, 
            s2$surv.inf, 
            s2$surv.sup))
    try(Net_Survival_Five_Year_age_3[i,] <-
          c(country_codes2[i,],
            #country_names_Survcan[i,1], 
            s3$surv, 
            s3$surv.inf, 
            s3$surv.sup))
    
    try(All_Cause_Survival_age_1[i,] <-
          c(country_codes2[i,],
            #country_names_Survcan[i,], 
            sp$surv, 
            sp$surv.inf, 
            sp$surv.sup))
    try(All_Cause_Survival_age_2[i,] <-
          c(country_codes2[i,],
            # country_names_Survcan[i,], 
            sp2$surv, 
            sp2$surv.inf, 
            sp2$surv.sup))
    try(All_Cause_Survival_age_3[i,] <-
          c(country_codes2[i,],
            # country_names_Survcan[i,], 
            sp3$surv, 
            sp3$surv.inf, 
            sp3$surv.sup))
  }
  
  #
  
  Age_names_all <- as.data.frame(c("15-49", "50-99", "Overall")) #
  
  Net_Survival_Five_Year_age_1 <- as.data.frame(Net_Survival_Five_Year_age_1)
  Net_Survival_Five_Year_age_2 <- as.data.frame(Net_Survival_Five_Year_age_2)
  Net_Survival_Five_Year_age_3 <-  as.data.frame(Net_Survival_Five_Year_age_3)
  
  All_Cause_Survival_age_1 <- as.data.frame(All_Cause_Survival_age_1)
  All_Cause_Survival_age_2 <- as.data.frame(All_Cause_Survival_age_2)
  All_Cause_Survival_age_3 <- as.data.frame(All_Cause_Survival_age_3)
  
  colnames(Net_Survival_Five_Year_age_1) <-
    c("country_code",
      #"country",
      "Five_Year_Net_Surv",
      "NS_Lower_CI",
      "NS_Upper_CI")
  colnames(Net_Survival_Five_Year_age_2) <-
    c("country_code",
      # "country",
      "Five_Year_Net_Surv",
      "NS_Lower_CI",
      "NS_Upper_CI")
  colnames(Net_Survival_Five_Year_age_3) <-
    c("country_code",
      # "country",
      "Five_Year_Net_Surv",
      "NS_Lower_CI",
      "NS_Upper_CI")
  colnames(All_Cause_Survival_age_1) <-
    c("country_code",
      # "country",
      "Five_Year_all_cause_Surv",
      "OS_Lower_CI",
      "OS_Upper_CI")
  colnames(All_Cause_Survival_age_2) <-
    c("country_code",
      #  "country",
      "Five_Year_all_cause_Surv",
      "OS_Lower_CI",
      "OS_Upper_CI")
  colnames(All_Cause_Survival_age_3) <-
    c("country_code",
      #  "country",
      "Five_Year_all_cause_Surv",
      "OS_Lower_CI",
      "OS_Upper_CI")
  
  
  
  Net_Survival_Five_Year_age_1 <-  Net_Survival_Five_Year_age_1 %>% mutate(age_cat = Age_names_all[1,])%>%filter(Five_Year_Net_Surv <=1)
  Net_Survival_Five_Year_age_2 <-  Net_Survival_Five_Year_age_2 %>% mutate(age_cat = Age_names_all[2,])%>%filter(Five_Year_Net_Surv <=1)
  Net_Survival_Five_Year_age_3 <-  Net_Survival_Five_Year_age_3 %>% mutate(age_cat = Age_names_all[3,])%>%filter(Five_Year_Net_Surv <=1)
  
  
  All_Cause_Survival_age_1 <- All_Cause_Survival_age_1 %>% mutate(age_cat = Age_names_all[1,])%>%filter(Five_Year_all_cause_Surv <=1)
  All_Cause_Survival_age_2 <- All_Cause_Survival_age_2 %>% mutate(age_cat = Age_names_all[2,])%>%filter(Five_Year_all_cause_Surv  <=1)
  All_Cause_Survival_age_3 <-  All_Cause_Survival_age_3 %>% mutate(age_cat = Age_names_all[3,])%>%filter(Five_Year_all_cause_Surv  <=1)
  
  
  Net_Survival_Five_Year <-  Net_Survival_Five_Year_age_1 %>% 
    full_join(Net_Survival_Five_Year_age_2)%>% 
    full_join(Net_Survival_Five_Year_age_3)%>% 
    mutate(country_code=as.numeric(country_code))%>% 
    left_join(country_names_Survcan)#%>% 
  #  filter(Five_Year_Net_Surv<1)
  
  All_Cause_Survival <-  All_Cause_Survival_age_1 %>% 
    full_join(All_Cause_Survival_age_2) %>% 
    full_join(All_Cause_Survival_age_3) %>%
    mutate(country_code=as.numeric(country_code))%>% 
    left_join(country_names_Survcan)#%>% 
  # filter(Five_Year_all_cause_Surv<1)
  
  
  
  NS_OS2 <-Net_Survival_Five_Year %>% left_join(All_Cause_Survival,
                                                by = c(
                                                  "country" = "country",
                                                  "country_code" = "country_code",
                                                  "age_cat" = "age_cat"))
  
  
  NS_OS2$country_code <- as.numeric(as.character(NS_OS2$country_code))
  
  bSURV$country_code <-as.numeric(as.character(bSURV$country_code))
  
  
  
  
  NS_OS <-
    NS_OS2 %>%
    #left_join(
    #   bSURV_age_cats,
    #   by = c(
    #     "country" = "country",
    #     "country_code" = "country_code",
    #     "age_cat" = "age_cat"
    #   )
    # ) %>%
    select(country_code, country,
           Five_Year_all_cause_Surv,OS_Lower_CI,OS_Upper_CI,
           Five_Year_Net_Surv,NS_Lower_CI,NS_Upper_CI,
           age_cat
    ) %>%
    distinct()%>%
    mutate(cancer_code=20)
  
  NS_OS$Five_Year_Net_Surv <- as.numeric(NS_OS$Five_Year_Net_Surv)
  NS_OS$NS_Lower_CI <- as.numeric(NS_OS$NS_Lower_CI)
  NS_OS$NS_Upper_CI <- as.numeric(NS_OS$NS_Upper_CI)
  NS_OS$Five_Year_all_cause_Surv <-
    as.numeric(NS_OS$Five_Year_all_cause_Surv)
  NS_OS$OS_Lower_CI <- as.numeric(NS_OS$OS_Lower_CI)
  NS_OS$OS_Upper_CI <- as.numeric(NS_OS$OS_Upper_CI)
  NS_OS$cancer <- as.factor(as.character(NS_OS$cancer))
  NS_OS$age_cat <- as.factor(NS_OS$age_cat)
  
  
  #########################
  #
  # AD Calculations
  #
  #########################
  
  #NS_OS <- read.csv("~/Documents/R_Projects/Data/Thai_NS_OS.csv")
  
  #Prepping PAFs data processing and combining by age group
  
  
  
  
  PAFs_age_Cat3 <- PAFs %>%
    #  filter(country_label == "Thailand") %>%%
    mutate(age_cat = case_when(age >= 4 & age <11 ~ "15-49",
                               age >= 11 ~ "50-99",
                               age<4 ~ "0-15")) %>%
    filter(age_cat != "0-15") %>%
    group_by(country_label, cancer_label, age_cat) %>%
    select(
      country_code,
      country_label,
      cancer_code,
      cancer_label,
      
      age_cat,
      cases,
      af.comb,
      cases.prev,
      cases.notprev,
      py,
      cases
    ) %>%
    group_by(country_label, cancer_label, age_cat) %>%
    mutate(    total_overall = sum(cases),
               py=sum(py))%>%
    
    select(-cases)%>%
    distinct()%>%
    as.data.frame()%>%
    droplevels()
  
  
  Seychelles_incidence<-bSURV%>%
    filter(country_code==690)%>%
    group_by(age_cat)%>%
    dplyr::mutate(total_overall=n())%>%
    select(country_code, cancer_code, total_overall)%>%
    mutate(country_label="Seychelles",
           cancer_label="Breast")%>%
    ungroup()%>%
    distinct()
  
  
  table(bSURV$age_cat)
  
  PAFs_age_Cat<- PAFs %>%
    #  filter(country_label == "Thailand") %>%
    mutate(age_cat = "Overall") %>%
    #  left_join(ES2%>%filter(sex==2, year==2012), by=c("country_code","age_cat","sex"))%>%
    group_by(country_label, cancer_label, age_cat) %>%
    select(
      country_code,
      country_label,
      cancer_code,
      cancer_label,
      age_cat,
      cases,
      af.comb,
      cases.prev,
      cases.notprev,
      py,
      cases
    ) %>%
    group_by(country_label, cancer_label, age_cat) %>%
    mutate(  total_overall = sum(cases),
             py=sum(py))%>%
    select(-cases)%>%
    distinct()%>%
    as.data.frame()%>%
    droplevels()%>%
    full_join(PAFs_age_Cat3)
  
  
  
  
  
  PAFs2 <- PAFs_age_Cat %>%
    #  full_join(PAFS_Overall) %>%
    as.data.frame() %>%
    droplevels()%>%
    group_by(country_label, cancer_label, age_cat) %>%
    mutate(total_age_prev = sum(cases.prev)) %>%
    mutate(af.comb.agecat = sum(cases.prev) / total_overall) %>%
    select(country_code,
           country_label,
           cancer_code,
           cancer_label,
           age_cat,
           af.comb.agecat,
           total_overall, py
           # cases
    ) %>%
    #  mutate(  total_overall = sum(cases))%>%
    # select(-cases)%>%
    distinct()%>%
    distinct() %>%
    arrange(cancer_label, age_cat) %>%
    ungroup()%>%
    full_join(Seychelles_incidence)%>%
    select(-country_label)%>%
    distinct()
  
  HDI_PR<-HDI%>%
    filter(country_code==840)%>%
    mutate(country_code=630,
           country_label="Puerto Rico")%>%
    distinct()
  
  
  HDI_Martinque<-HDI%>%
    filter(country_label=="France")%>%
    mutate(country_code=474,
           country_label="Martinique")%>%
    distinct()
  
  HDI<-HDI%>%
    full_join(HDI_Martinque)%>%
    full_join(HDI_PR)
  
  
  #Calculating references - Sweden 2017-2021 survival scaled by population size: 
  
  ref_swe2 <- data.frame(age=c(18,	17,	16,	15,	14,	13,	12,	11,	10,	9,	8,	7,	6,	5,	4,	3,	2,	1),
                         pop=c(169094,	157277,	244475,	278834,	272046,	283956,	322032,	328522,	326852,	311807,	324093,	366224,	342303,	271506,	282470,	303163,	301757,	287625),
                         surv_ref=c(82.7, 82.7,	93.1,	93.1,	95.4,	95.4,	94.8,	94.8,	94,	94,	94,	94,	94,	94,	94,	94,	94,	94)
  )
  
  
  
  ref_swe_overall<-ref_swe2%>%
    filter(age>=4)%>%
    dplyr::mutate(age_cat = "Overall")%>%
    select(-age)%>%
    group_by(age_cat)%>%
    mutate(surv_ref=sum(surv_ref*pop)/sum(pop)/100)%>%
    select(-pop)%>%
    ungroup()%>%
    distinct()
  
  
  ref_swe<-ref_swe2%>%
    filter(age>=4)%>%
    dplyr::mutate(age_cat = case_when(
      age>=4 & age<11~ "15-49",
      age>=11 & age<=18~ "50-99",
    ))%>%
    select(-age)%>%
    group_by(age_cat)%>%
    mutate(surv_ref=sum(surv_ref*pop)/sum(pop)/100)%>%
    select(-pop)%>%
    ungroup()%>%
    distinct()%>%
    full_join(ref_swe_overall)
  
  
  
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
    filter(Five_Year_all_cause_Surv < 1, Five_Year_Net_Surv < 1) %>% #filtering out values for which the algorithm failed
    left_join(ref_swe, by=c("age_cat"))%>%
    left_join(HDI %>% select(country_code, hdi_group), by = c("country_code")) %>%
    ungroup() %>%
    group_by(age_cat, hdi_group) %>%
    mutate(#median_ref = median(Five_Year_Net_Surv),
      max_ref = max(Five_Year_Net_Surv)) %>%
    dplyr::rename("country_label" = "country") %>%
    ungroup() %>%
    select(-hdi_group)%>%
    filter((country_label=="Kenya"&age_cat=="50-99")==FALSE) #Failed to converge
  
  
  ####################################################################################
  #
  #
  # Rutherford Avoidable Deaths calculations 
  #
  #
  ####################################################################################
  
  
  Avoidable_Deaths <- NS_OS_PAF %>%
    mutate(
      es=Five_Year_all_cause_Surv/Five_Year_Net_Surv,
      AD = (surv_ref - Five_Year_Net_Surv) * es  * total_overall,
      AD_Lower = (surv_ref - NS_Upper_CI) *   es * total_overall,
      AD_Upper = (surv_ref - NS_Lower_CI) *  es * total_overall,
      AD_unavoid = total_overall * (1 - surv_ref * es),
      total_deaths = AD + AD_unavoid,
      # #Median in each HDI group as reference
      # AD_med = (median_ref - Five_Year_Net_Surv) * es  * total_overall,
      # AD_Lower_med = (median_ref - NS_Upper_CI) *   es * total_overall,
      # AD_Upper_med = (median_ref - NS_Lower_CI) *  es * total_overall,
      # AD_unavoid_med = total_overall * (1 - median_ref * es),
      #max in each HDI group as reference
      # AD_max = (max_ref - Five_Year_Net_Surv) * es  * total_overall,
      # AD_Lower_max = (max_ref - NS_Upper_CI) *   es * total_overall,
      # AD_Upper_max = (max_ref - NS_Lower_CI) *  es * total_overall,
      # AD_unavoid_max = total_overall * (1 - max_ref * es),
      # 
      # #Changing to have absolute max
      AD_max = (1 - Five_Year_Net_Surv) * es  * total_overall,
      AD_Lower_max = (1 - NS_Upper_CI) *   es * total_overall,
      AD_Upper_max = (1 - NS_Lower_CI) *  es * total_overall,
      AD_unavoid_max = total_overall * (1 - 1 * es),
      total_deaths_max = AD_max + AD_unavoid_max,
      
      
      #accounting for cases where the net survival is equal (or higher) than the references
      AD  = case_when(
        Five_Year_Net_Surv >= surv_ref ~ 0,
        Five_Year_Net_Surv < surv_ref ~ AD),
      AD_Lower  = case_when(
        Five_Year_Net_Surv >= surv_ref ~ 0,
        Five_Year_Net_Surv < surv_ref ~ AD_Lower),
      AD_Upper  = case_when(
        Five_Year_Net_Surv >= surv_ref ~ 0,
        Five_Year_Net_Surv < surv_ref ~ AD_Upper),
      # AD_med = case_when(
      #   Five_Year_Net_Surv >= median_ref ~ 0,
      #   Five_Year_Net_Surv < median_ref ~ AD_med),
      # AD_Lower_med = case_when(
      #   Five_Year_Net_Surv >= median_ref ~ 0,
      #   Five_Year_Net_Surv < median_ref ~ AD_Lower_med),
      # AD_Upper_med = case_when(
      #   Five_Year_Net_Surv >= median_ref ~ 0,
      #   Five_Year_Net_Surv < median_ref ~ AD_Upper_med),
      #for max by HDI - comment for max NS =1 scenario
      
      # AD_max = case_when(
      #   Five_Year_Net_Surv == max_ref ~ 0,
      #   Five_Year_Net_Surv != max_ref ~ AD_max),
      # AD_Lower_max = case_when(
      #   Five_Year_Net_Surv == max_ref ~ 0,
      #   Five_Year_Net_Surv != max_ref ~ AD_Lower_max),
      # AD_Upper_max = case_when(
      #   Five_Year_Net_Surv == max_ref ~ 0,
      #   Five_Year_Net_Surv != max_ref ~ AD_Upper_max,
  #    )
  ) %>%
    #fixing any negative values so they aren't in the total
    select(#"es",
      "country_code", "country_label", "age_cat",
      "cancer_code", "cancer_label",
      "AD","AD_Lower", "AD_Upper", #"AD_unavoid",
      # "AD_med", "AD_Lower_med", "AD_Upper_med",
      "AD_max",  "AD_Lower_max",  "AD_Upper_max",#"AD_unavoid_max",
      "total_deaths", "total_overall"#, "total_deaths_max"
      )
  
  
  #Keeping subgroups for which both aren't available seperate from the overall group (to filter countries from the overall)
  AD_countries_low <- Avoidable_Deaths %>% 
    filter(age_cat == "15-49") %>% 
    filter(!country_label%in%c("Kenya","Namibia", "Mauritius", "Seychelles", "South Africa", "Iran"))%>%
    select(country_label)
  
  AD_countries_upp <- Avoidable_Deaths %>% 
    filter(age_cat == "50-99") %>% 
    filter(!country_label%in%c("Namibia", "Kenya"))%>%
    select(country_label)
  
  
  AD_countries_overall <-  Avoidable_Deaths %>% 
    filter(age_cat == "Overall") %>%
    filter(country_label%in%c("Kenya","Namibia", "Mauritius", "Seychelles", "South Africa", "Iran"))%>%
    select(country_label)
  
  
  no_overall_upp <-  AD_countries_low %>% filter(!country_label %in% AD_countries_upp$country_label)
  
  no_overall_low <-AD_countries_upp %>% filter(!country_label %in% AD_countries_low$country_label)
  no_overall_o1 <-  AD_countries_overall %>% filter(!country_label %in% AD_countries_low$country_label)
  no_overall_o2 <- AD_countries_overall %>% filter(!country_label %in% AD_countries_upp$country_label)
  
  
  
  # Globocan population data 
  
  pop202021 <- pop2020%>%
    as.data.frame()%>%
    dplyr::filter(sex==2)%>%
    select(-sex)%>%
    dplyr::filter(age>3)%>%
    dplyr::mutate(age_cat = case_when(
      age>=4 & age<11~ "15-49",
      age>=11 & age<=18~ "50-99",
    ))%>%
    select(-age)%>%
    dplyr::group_by(country_code, age_cat)%>%
    dplyr::mutate(py=sum(py))%>%
    distinct()%>%
    select(-country_label)
  
  pop20202 <- pop2020%>%
    as.data.frame()%>%
    dplyr::filter(sex==2)%>%
    select(-sex)%>%
    dplyr::filter(age>3)%>%
    dplyr::mutate(age_cat = "Overall")%>%
    select(-age)%>%
    dplyr::group_by(country_code, age_cat)%>%
    dplyr::mutate(py=sum(py))%>%
    distinct()%>%
    select(-country_label)%>%
    full_join(pop202021)
  
  
  # Weights
  w2 <- c(12000, 10000, 9000, 9000, 8000, 8000, 6000, 6000, 6000,6000, 5000, 4000, 4000, 3000, 2000, 1000,500,500) #SEGI_pop weights
  age <- c(1:18)
  weights22 <- data.frame(w2, age)
  
  
  weights21 <- weights22%>%
    dplyr::filter(age>3)%>%
    dplyr::mutate(age_cat = case_when(
      age>=4 & age<11~ "15-49",
      age>=11 & age<=18~ "50-99",
    ))%>%
    select(-age)%>%
    dplyr::mutate(total=sum(w2))%>%
    dplyr::group_by(age_cat)%>%
    dplyr::mutate(w=sum(w2)/total)%>%
    select(-w2, -total)%>%
    distinct()
  
  
  weights2 <- weights22%>%
    dplyr::filter(age>3)%>%
    dplyr::mutate(age_cat = "Overall")%>%
    select(-age)%>%
    dplyr::mutate(total=sum(w2))%>%
    dplyr::group_by(age_cat)%>%
    dplyr::mutate(w=sum(w2)/total)%>%
    select(-w2, -total)%>%
    distinct()%>%
    full_join(weights21)
  
  
  # All countries data set 
  
  Avoidable_Deaths_overall <- Avoidable_Deaths %>%
    as.data.frame() %>%
    
 #   select(-AD_unavoid, -AD_unavoid_max)%>%
    # left_join(pop20202, by=c("country_code", "age_cat"))%>%
    #left_join(weights2, by=c("age_cat"))%>%
    filter(age_cat != "Overall") %>%
    filter(!country_label%in%c("Kenya","Namibia", "Mauritius", "Seychelles", "South Africa", "Iran"))%>%
    mutate(age_cat = "Overall") %>%
    
    ungroup() %>%
    group_by(country_code, cancer_code, age_cat) %>%
    dplyr::mutate(         
      # #ASRs
      # AD.asr=sum(AD/py*100000*w),
      # AD_Lower.asr=sum(AD_Lower/py*100000*w),
      # AD_Upper.asr=sum(AD_Upper/py*100000*w),
      total_overall = sum(total_overall),
      AD = sum(AD),
      AD_Lower = sum(AD_Lower),
      AD_Upper = sum(AD_Upper),
      # AD_med = sum(AD_med),
      # AD_Lower_med = sum(AD_Lower_med),
      # AD_Upper_med = sum(AD_Upper_med),
      AD_max = sum(AD_max),
      AD_Lower_max = sum(AD_Lower_max),
      AD_Upper_max = sum(AD_Upper_max),
 #     AD_unavoid = sum(AD_unavoid),
      total_deaths = sum(total_deaths)) %>%
    select( country_code,
            country_label,
            cancer_code,
            cancer_label,
            total_overall,
            AD,
            AD_Lower,
            AD_Upper,
          #  AD_unavoid,
            AD_max,
            AD_Lower_max,
            AD_Upper_max,
         #   AD_unavoid_max,
            total_deaths,
            #    AD.asr,
            #   AD_Lower.asr,
            #    AD_Upper.asr
    )%>%
    distinct() %>%
    as.data.frame()
  
  
  Avoidable_Deaths_overall2 <- Avoidable_Deaths %>%
    as.data.frame() %>%
    left_join(pop20202, by=c("country_code", "age_cat"))%>%
    left_join(weights2, by=c("age_cat"))%>%
      filter(age_cat == "Overall") %>% 
    filter(country_label%in%c("Kenya","Namibia", "Mauritius", "Seychelles", "South Africa", "Iran"))%>%
    group_by(country_code, cancer_code, age_cat) %>%
    select( country_code,
            country_label,
            cancer_code,
            cancer_label,
            total_overall,
            AD,
            AD_Lower,
            AD_Upper,
            
            AD_max,
            AD_Lower_max,
            AD_Upper_max,
          #  AD_unavoid,
          #  AD_unavoid_max,
            total_deaths,
    )%>%
    distinct() %>%
    as.data.frame()
  
  
  Avoidable_Deaths_age_cat <- Avoidable_Deaths %>%
   # select(-AD_unavoid,-AD_unavoid_max)%>%
    filter(age_cat!="Overall")%>%
    filter((age_cat=="15-49" & country_label%in%c("Kenya","Namibia", "Mauritius", "Seychelles", "South Africa", "Iran"))==FALSE)%>%
    filter((age_cat=="50-99" & country_label%in%c("Kenya","Namibia"))==FALSE)%>%
    group_by(country_code, cancer_code, age_cat) %>%
    mutate(AD = sum(AD)) %>%
    full_join(Avoidable_Deaths_overall) %>%
    full_join(Avoidable_Deaths_overall2) %>%
    #select(-py, -w)%>%
    left_join(pop20202, by=c("country_code", "age_cat"))%>%
    left_join(weights2, by=c("age_cat"))%>%
    group_by(country_code, cancer_code, age_cat) %>%
    dplyr::mutate(         
      #ASRs
      # AD.asr=sum(AD/py*100000*w),
      # AD_Lower.asr=sum(AD_Lower/py*100000*w),
      # AD_Upper.asr=sum(AD_Upper/py*100000*w))%>%
      # age specific crude rates
      AD.rate=sum(AD/py*100000),
      AD_Lower.rate=sum(AD_Lower/py*100000),
      AD_Upper.rate=sum(AD_Upper/py*100000),
      AD_max.rate=sum(AD_max/py*100000),
      AD_Lower_max.rate=sum(AD_Lower_max/py*100000),
      AD_Upper_max.rate=sum(AD_Upper_max/py*100000))%>%
    dplyr::mutate(pAD = AD / total_deaths) %>%
    dplyr::mutate(pAD_Lower = AD_Lower / total_deaths) %>%
    dplyr::mutate(pAD_Upper = AD_Upper / total_deaths) %>%
    # dplyr::mutate(pAD_med = AD_med / total_deaths) %>%
    # dplyr::mutate(pAD_Lower_med = AD_Lower_med / total_deaths) %>%
    # dplyr::mutate(pAD_Upper_med = AD_Upper_med / total_deaths) %>%
    dplyr::mutate(pAD_max = AD_max / total_deaths) %>%
    dplyr::mutate(pAD_Lower_max = AD_Lower_max / total_deaths) %>%
    dplyr::mutate(pAD_Upper_max = AD_Upper_max / total_deaths) %>%
  #  select(-AD_unavoid,-AD_unavoid_max)%>%
    distinct() %>%
    as.data.frame()
  
  #AD by HDI group 
  
  HDI
  
  
  AD_HDI <- Avoidable_Deaths_age_cat %>%
    left_join(HDI %>% select(country_code, hdi_group), by = c("country_code")) %>%
    select(-country_code#,
           #-AD_unavoid, -AD_unavoid_max
           ) %>%
    group_by(hdi_group, age_cat) %>%
    dplyr::mutate(         
      #ASRs
      # AD.asr=sum(AD/py*100000*w),
      # AD_Lower.asr=sum(AD_Lower/py*100000*w),
      # AD_Upper.asr=sum(AD_Upper/py*100000*w))%>%
      
      # age specific crude rates
      AD.rate=sum(AD/py*100000),
      AD_Lower.rate=sum(AD_Lower/py*100000),
      AD_Upper.rate=sum(AD_Upper/py*100000),
      AD_max.rate=sum(AD_max/py*100000),
      AD_Lower_max.rate=sum(AD_Lower_max/py*100000),
      AD_Upper_max.rate=sum(AD_Upper_max/py*100000))%>%
    dplyr::summarise(
      hdi_group,
      cancer_code,
      cancer_label,
      total_overall = sum(total_overall),
      AD = sum(AD),
      AD_Lower = sum(AD_Lower),
      AD_Upper = sum(AD_Upper),
      # AD_med = sum(AD_med),
      # AD_Lower_med = sum(AD_Lower_med),
      # AD_Upper_med = sum(AD_Upper_med),
      AD_max = sum(AD_max),
      AD_Lower_max = sum(AD_Lower_max),
      AD_Upper_max = sum(AD_Upper_max),
      total_deaths = sum(total_deaths),
      py=sum(py),
      #AD.asr, AD_Lower.asr, AD_Upper.asr,
      AD.rate, AD_Lower.rate, AD_Upper.rate,
      AD_max.rate, AD_Lower_max.rate, AD_Upper_max.rate,
      ) %>%
    distinct() %>%
    dplyr::mutate(pAD = AD / total_deaths) %>%
    dplyr::mutate(pAD_Lower = AD_Lower / total_deaths) %>%
    dplyr::mutate(pAD_Upper = AD_Upper / total_deaths) %>%
    # dplyr::mutate(pAD_med = AD_med / total_deaths) %>%
    # dplyr::mutate(pAD_Lower_med = AD_Lower_med / total_deaths) %>%
    # dplyr::mutate(pAD_Upper_med = AD_Upper_med / total_deaths) %>%
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
  
  continents <- HDI_Region_Mapping %>%
    dplyr::filter(
      country_code >= 910 & country_code <= 931 |
        country_code == 905 | country_code == 906 |
        country_code == 954 | country_code == 957
    ) %>%
    select(continent)%>%distinct()%>%
    mutate(country_label= case_when(continent==1 ~ "Africa",
                                    continent==4 ~ "Asia",
                                    continent==2 ~ "Latin America"))
  
  
  
  AD_continent <- Avoidable_Deaths_age_cat %>%
    select(-country_label#, 
           #-AD_unavoid, -AD_unavoid_max
           ) %>%
    left_join(HDI_Region_Mapping2) %>%
    # left_join(areas) %>%
    select(-country_code, -area) %>%
    group_by(continent, age_cat) %>%
    dplyr::mutate(         
      #ASRs
      # AD.asr=sum(AD/py*100000*w),
      # AD_Lower.asr=sum(AD_Lower/py*100000*w),
      # AD_Upper.asr=sum(AD_Upper/py*100000*w))%>%
      
      # age specific crude rates
      AD.rate=sum(AD/py*100000),
      AD_Lower.rate=sum(AD_Lower/py*100000),
      AD_Upper.rate=sum(AD_Upper/py*100000),
      AD_max.rate=sum(AD_max/py*100000),
      AD_Lower_max.rate=sum(AD_Lower_max/py*100000),
      AD_Upper_max.rate=sum(AD_Upper_max/py*100000))%>%
    
    dplyr::mutate(
      continent,
      # country_label,
      cancer_code,
      cancer_label,
      total_overall = sum(total_overall),
      AD = sum(AD),
      AD_Lower = sum(AD_Lower),
      AD_Upper = sum(AD_Upper),
      # AD_med = sum(AD_med),
      # AD_Lower_med = sum(AD_Lower_med),
      # AD_Upper_med = sum(AD_Upper_med),
      AD_max = sum(AD_max),
      AD_Lower_max = sum(AD_Lower_max),
      AD_Upper_max = sum(AD_Upper_max),
      total_deaths = sum(total_deaths),
      # AD.asr,
      # AD_Lower.asr,
      # AD_Upper.asr,
  
      AD.rate,
      AD_Lower.rate,
      AD_Upper.rate,
      AD_max.rate,
      AD_Lower_max.rate,
      AD_Upper_max.rate,
    ) %>%
    distinct() %>%
    dplyr::mutate(pAD = AD / total_deaths) %>%
    dplyr::mutate(pAD_Lower = AD_Lower / total_deaths) %>%
    dplyr::mutate(pAD_Upper = AD_Upper / total_deaths) %>%
    # dplyr::mutate(pAD_med = AD_med / total_deaths) %>%
    # dplyr::mutate(pAD_Lower_med = AD_Lower_med / total_deaths) %>%
    # dplyr::mutate(pAD_Upper_med = AD_Upper_med / total_deaths) %>%
    dplyr::mutate(pAD_max = AD_max / total_deaths) %>%
    dplyr::mutate(pAD_Lower_max = AD_Lower_max / total_deaths) %>%
    dplyr::mutate(pAD_Upper_max = AD_Upper_max / total_deaths) %>%
    select(-py, -w)%>%
    distinct() %>%
    as.data.frame()%>%
    left_join(continents,by=c("continent"))
  
  
  #Summing by country, region, and such
  AD_all<-
    Avoidable_Deaths_age_cat %>% #Missing countries for which no overall was calculated. Here the overall will be calculated by running the overall survival
    # filter(age_cat!="Overall")%>%
    # ungroup()%>%
    # mutate(age_cat="Overall")%>%
    select(#-AD.asr, -AD_Lower.asr, -AD_Upper.asr,
           -AD.rate, -AD_Lower.rate, -AD_Upper.rate,
           -AD_max.rate,
           -AD_Lower_max.rate,
           -AD_Upper_max.rate,
         #  -AD_unavoid,
          # -AD_unavoid_max,
           )%>%
    as.data.frame() %>%
    mutate(country_code = 1000) %>%
    mutate(country_label = "All countries") %>%
    group_by(age_cat) %>%
    mutate(AD = sum(AD, na.rm = TRUE)) %>%
    mutate(AD_Lower = sum(AD_Lower, na.rm = TRUE)) %>%
    mutate(AD_Upper = sum(AD_Upper, na.rm = TRUE)) %>%
    # mutate(AD_med = sum(AD_med, na.rm = TRUE)) %>%
    # mutate(AD_Lower_med = sum(AD_Lower_med, na.rm = TRUE)) %>%
    # mutate(AD_Upper_med = sum(AD_Upper_med, na.rm = TRUE)) %>%
    mutate(AD_max = sum(AD_max, na.rm = TRUE)) %>%
    mutate(AD_Lower_max = sum(AD_Lower_max, na.rm = TRUE)) %>%
    mutate(AD_Upper_max = sum(AD_Upper_max, na.rm = TRUE)) %>%
    mutate(total_overall = sum(total_overall, na.rm = TRUE)) %>%
    mutate(total_deaths = sum(total_deaths, na.rm = TRUE)) %>%
    dplyr::mutate(pAD = AD / total_deaths) %>%
    dplyr::mutate(pAD_Lower = AD_Lower / total_deaths) %>%
    dplyr::mutate(pAD_Upper = AD_Upper / total_deaths) %>%
    # dplyr::mutate(pAD_med = AD_med / total_deaths) %>%
    # dplyr::mutate(pAD_Lower_med = AD_Lower_med / total_deaths) %>%
    # dplyr::mutate(pAD_Upper_med = AD_Upper_med / total_deaths) %>%
    dplyr::mutate(pAD_max = AD_max / total_deaths) %>%
    dplyr::mutate(pAD_Lower_max = AD_Lower_max / total_deaths) %>%
    dplyr::mutate(pAD_Upper_max = AD_Upper_max / total_deaths) %>%
    mutate(py=sum(py))%>%
    distinct()%>%
    dplyr::mutate(         
      #ASRs
      # AD.asr=sum(AD/py*100000*w),
      # AD_Lower.asr=sum(AD_Lower/py*100000*w),
      # AD_Upper.asr=sum(AD_Upper/py*100000*w))%>%
      
      # age specific crude rates
      AD.rate=sum(AD/py*100000),
      AD_Lower.rate=sum(AD_Lower/py*100000),
      AD_Upper.rate=sum(AD_Upper/py*100000),
      AD_max.rate=sum(AD_max/py*100000),
      AD_Lower_max.rate=sum(AD_Lower_max/py*100000),
      AD_Upper_max.rate=sum(AD_Upper_max/py*100000))%>%
    ungroup() %>%
    select(-py, -w)%>%
    ungroup()%>%
    distinct() %>%
    as.data.frame()
  
  
  Avoidable_Deaths_age_cat2 <- Avoidable_Deaths_age_cat %>%
    select(-py,-w)%>%
    dplyr::mutate(across(6:13, round,-1))%>%
    dplyr::mutate(across(14:19,round, 1))%>%
    dplyr::mutate(across(20:25, round, 3) * 100) %>% #dplyr::mutate to show proportion as percentage in export
    select(
      "country_code",
      "country_label",
      "age_cat",
      "cancer_code", "cancer_label",
      "AD", "AD_Lower", "AD_Upper",
      "pAD", "pAD_Lower", "pAD_Upper",
      #"AD.asr", "AD_Lower.asr", "AD_Upper.asr",
      "AD.rate", "AD_Lower.rate", "AD_Upper.rate",
  
      
      #  "AD_med", "AD_Lower_med","AD_Upper_med", 
      # "pAD_med","pAD_Lower_med", "pAD_Upper_med",
      "AD_max", "AD_Lower_max", "AD_Upper_max",
      "pAD_max", "pAD_Lower_max", "pAD_Upper_max",
      "AD_max.rate", "AD_Lower_max.rate", "AD_Upper_max.rate",
      "total_deaths"
    ) %>%
    arrange(age_cat, country_label)
  
  
  
  AD_HDI2 <- AD_HDI %>%
    dplyr::rename("country_label" = "hdi_group") %>%
    dplyr::mutate(country_label = as.character(country_label)) %>%
    select(
      "country_label","age_cat",
      "cancer_code","cancer_label",
      "AD",  "AD_Lower",  "AD_Upper",
      #"AD_med",  "AD_Lower_med",  "AD_Upper_med",
      "AD_max",  "AD_Lower_max",  "AD_Upper_max",
      "total_deaths",
      "pAD",  "pAD_Lower",  "pAD_Upper",
      # "pAD_med",  "pAD_Lower_med",  "pAD_Upper_med",
      "pAD_max",  "pAD_Lower_max",  "pAD_Upper_max",
     # "AD.asr",  "AD_Lower.asr",  "AD_Upper.asr",
      "AD.rate", "AD_Lower.rate", "AD_Upper.rate",
     "AD_max.rate", "AD_Lower_max.rate", "AD_Upper_max.rate",
      ) %>%
    dplyr::mutate(across(5:11, round,-1)) %>%
    dplyr::mutate(across(12:17, round, 3) * 100) %>% #dplyr::mutate to show proportion as percentage in export
    dplyr::mutate(across(18:23,round, 1))%>%
    select(
      "country_label", "age_cat",
      "cancer_code", "cancer_label",
      "AD", "AD_Lower", "AD_Upper",
      "pAD", "pAD_Lower", "pAD_Upper",
     # "AD.asr", "AD_Lower.asr", "AD_Upper.asr",
      "AD.rate", "AD_Lower.rate", "AD_Upper.rate",
      #  "AD_med", "AD_Lower_med","AD_Upper_med", 
      #   "pAD_med","pAD_Lower_med", "pAD_Upper_med",
      "AD_max", "AD_Lower_max", "AD_Upper_max",
      "pAD_max", "pAD_Lower_max", "pAD_Upper_max",
     "AD_max.rate", "AD_Lower_max.rate", "AD_Upper_max.rate",
      "total_deaths") %>%
    arrange(age_cat, country_label)
  
  
  AD_continent2 <- AD_continent %>%
    select("continent","country_label", "age_cat",
           "cancer_code","cancer_label",
           "AD",  "AD_Lower", "AD_Upper",
           #  "AD_med", "AD_Lower_med", "AD_Upper_med",
           "AD_max", "AD_Lower_max", "AD_Upper_max",
           "total_deaths", 
           "pAD", "pAD_Lower", "pAD_Upper",
           #   "pAD_med", "pAD_Lower_med", "pAD_Upper_med",
           "pAD_max","pAD_Lower_max",  "pAD_Upper_max",
       #    "AD.asr", "AD_Lower.asr", "AD_Upper.asr",
           "AD.rate", "AD_Lower.rate", "AD_Upper.rate",
       "AD_max.rate", "AD_Lower_max.rate", "AD_Upper_max.rate",) %>%
    dplyr::mutate(across(6:12, round,-1)) %>%
    dplyr::mutate(across(13:18, round, 3) * 100) %>% #dplyr::mutate to show proportion as percentage in export
    dplyr::mutate(across(19:24,round, 1))%>%
    select(
      "continent", "country_label","age_cat",
      "cancer_code", "cancer_label",
      "AD", "AD_Lower", "AD_Upper",
      "pAD", "pAD_Lower", "pAD_Upper",
      #"AD.asr", "AD_Lower.asr", "AD_Upper.asr",
      "AD.rate", "AD_Lower.rate", "AD_Upper.rate",
      # "AD_med", "AD_Lower_med","AD_Upper_med", 
      #"pAD_med","pAD_Lower_med", "pAD_Upper_med",
      "AD_max", "AD_Lower_max", "AD_Upper_max",
      "pAD_max", "pAD_Lower_max", "pAD_Upper_max",
      "AD_max.rate", "AD_Lower_max.rate", "AD_Upper_max.rate",
      "total_deaths") %>%
    arrange(age_cat, continent)
  
  
  
  AD_all2 <- AD_all %>%
    dplyr::mutate(across(6:13, round,-1))%>%
    dplyr::mutate(across(14:19,round, 3)*100)%>%
    dplyr::mutate(across(20:25, round, 1)) %>% #dplyr::mutate to show proportion as percentage in export
    select(
      "country_code", "country_label", "age_cat",
      "cancer_code", "cancer_label",
      "AD", "AD_Lower", "AD_Upper",
      "pAD", "pAD_Lower", "pAD_Upper",
      #"AD.asr", "AD_Lower.asr", "AD_Upper.asr",
      "AD.rate", "AD_Lower.rate", "AD_Upper.rate",
      #  "AD_med", "AD_Lower_med","AD_Upper_med", 
      # "pAD_med","pAD_Lower_med", "pAD_Upper_med",
      "AD_max", "AD_Lower_max", "AD_Upper_max",
      "pAD_max", "pAD_Lower_max", "pAD_Upper_max",
      "AD_max.rate", "AD_Lower_max.rate", "AD_Upper_max.rate",
      "total_deaths") %>%
    arrange(age_cat, country_label)
  
  AD_2ages <- AD_all2
  
  
  AD_table_main <- AD_continent2 %>%
    full_join(AD_HDI2) %>%
    full_join(AD_all2) %>%
    distinct()%>%
    mutate(
      "Number 1" = paste0(AD, " (", AD_Lower, ", ", AD_Upper, ")"),
      "Proportion 1 (%)" = paste0(pAD, " (", pAD_Lower, ", ", pAD_Upper, ")"),
      #"ASR 1" = paste0(AD.asr, " (", AD_Lower.asr, ", ", AD_Upper.asr, ")"),
      "Rate 1" = paste0(AD.rate, " (", AD_Lower.rate, ", ", AD_Upper.rate, ")"),
      #  "Number 2" = paste0(AD_med, " (", AD_Lower_med, ", ", AD_Upper_med, ")"),
      #  "Proportion 2 (%)" = paste0(pAD_med, " (", pAD_Lower_med, ", ", pAD_Upper_med, ")"),
      "Number 3" = paste0(AD_max, " (", AD_Lower_max, ", ", AD_Upper_max, ")"),
      "Proportion 3 (%)" = paste0(pAD_max, " (", pAD_Lower_max, ", ", pAD_Upper_max, ")"),
      "Rate 3" = paste0(AD_max.rate, " (", AD_Lower_max.rate, ", ", AD_Upper_max.rate, ")")
    ) %>%
    dplyr::rename("Country" = "country_label") %>%
    dplyr::rename("Age Group" = "age_cat") %>%
    select(
      "Country",
      "Age Group",
      "Number 1",
      "Proportion 1 (%)",
      #"ASR 1",
      "Rate 1",
      # "Number 2",
      #  "Proportion 2 (%)",
      "Number 3",
      "Proportion 3 (%)",
      "Rate 3",
    )
  
  Avoidable_Deaths_age_cattest<-  Avoidable_Deaths_age_cat%>%filter(age_cat=="Overall")
  sum(Avoidable_Deaths_age_cattest$AD)
  
  
  AD_table_countries <- Avoidable_Deaths_age_cat2 %>%
    mutate(
      "Number 1" = paste0(AD, " (", AD_Lower, ", ", AD_Upper, ")"),
      "Proportion 1 (%)" = paste0(pAD, " (", pAD_Lower, ", ", pAD_Upper, ")"),
      #"ASR 1" = paste0(AD.asr, " (", AD_Lower.asr, ", ", AD_Upper.asr, ")"),
      "Rate 1" = paste0(AD.rate, " (", AD_Lower.rate, ", ", AD_Upper.rate, ")"),
      #  "Number 2" = paste0(AD_med, " (", AD_Lower_med, ", ", AD_Upper_med, ")"),
      #  "Proportion 2 (%)" = paste0(pAD_med, " (", pAD_Lower_med, ", ", pAD_Upper_med, ")"),
      "Number 3" = paste0(AD_max, " (", AD_Lower_max, ", ", AD_Upper_max, ")"),
      "Proportion 3 (%)" = paste0(pAD_max, " (", pAD_Lower_max, ", ", pAD_Upper_max, ")"),
      "Rate 3" = paste0(AD_max.rate, " (", AD_Lower_max.rate, ", ", AD_Upper_max.rate, ")")
    ) %>%
    dplyr::rename("Country" = "country_label") %>%
    dplyr::rename("Age Group" = "age_cat") %>%
    select(
      "Country",
      "Age Group",
      "Number 1",
      "Proportion 1 (%)",
     # "ASR 1",
      "Rate 1",
      # "Number 2",
      #  "Proportion 2 (%)",
      "Number 3",
      "Proportion 3 (%)",
     "Rate 3",
    )
  
  
  AD_table_countries
  
  
  
  check_ncountries <- Avoidable_Deaths_age_cat %>%
    group_by(age_cat) %>%
    dplyr::mutate(n_countries = n()) %>%
    select(age_cat, n_countries) %>%
    distinct()
  check_ncountries
  
  #Checking number of countries in each HDI group
  
  AD_HDI_n <- Avoidable_Deaths_age_cat %>%
    left_join(HDI %>% select(country_code, hdi_group), by = c("country_code")) %>%
    group_by(age_cat, hdi_group) %>%
    select(age_cat, hdi_group, country_code, country_label) %>%
    dplyr::mutate(n_countries = n())
  AD_HDI_n
  
  #Checking number of countries in each subregion
  AD_continent_n <- Avoidable_Deaths_age_cat %>%
    select(-country_label) %>%
    left_join(HDI_Region_Mapping2) %>%
    left_join(areas) %>%
    select(-country_code) %>%
    group_by(age_cat, area) %>%
    select(age_cat, area, country_label) %>%
    dplyr::mutate(n_countries = n()) %>%
    distinct()
  AD_continent_n
  
  #Checking number of countries in each region - seems more reasonable to group by region as we avoid regions represented by one country...
  
  AD_continent_n <- Avoidable_Deaths_age_cat %>%
    # select(-country_label)%>%
    left_join(HDI_Region_Mapping2, by = c("country_code")) %>%
    # left_join(areas)%>%
    select(-country_code) %>%
    group_by(age_cat, continent) %>%
    select(country_label, age_cat, continent) %>%
    dplyr::mutate(n_countries = n()) %>%
    distinct()
  
  AD_continent_n
  
  
  
  write.csv2(AD_table_main, "\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\Breast Cancer\\Results\\table_main_Survcan.csv", row.names = F)
  
  write.csv2(AD_table_countries, "\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\Breast Cancer\\Results\\table_countries_Survcan.csv", row.names = F)
  
  
  
  
  # Numbers for the text
  
  # total patients in SURVCAN-3 for breast cancer
  n_patients<-nrow(bcan_SURV)
  n_patients
  # Total # after inclusion criteria
  includ<-nrow(bSURV_Overall)
  includ
  #percentage included
  
  includ/n_patients*100
  
  
  #Seeing statistically significant differences between age groups in the country
  
age_ss_low<-Avoidable_Deaths_age_cat2%>%
  group_by(country_code)%>%
  filter(age_cat=="15-49")%>%
  select(country_code, country_label, age_cat, pAD, pAD_Lower, pAD_Upper)%>%
  rename("age1"="age_cat",
         "pAD_age1"="pAD",
         "pAD_age1_low"="pAD_Lower",
         "pAD_age1_upp"="pAD_Upper")
  
  
  
age_ss<-Avoidable_Deaths_age_cat2%>%
  group_by(country_code)%>%
  filter(age_cat=="50-99")%>%
  select(country_code, country_label, age_cat, pAD, pAD_Lower, pAD_Upper)%>%
  rename("age2"="age_cat",
         "pAD_age2"="pAD",
         "pAD_age2_low"="pAD_Lower",
         "pAD_age2_upp"="pAD_Upper")%>%
  left_join(age_ss_low)%>%
  filter(pAD_age1_upp<=pAD_age2_low)
  
age_ss$country_label

age_ss_low

#Comparing lower and higher age group
  
age_ss_nonsf<-Avoidable_Deaths_age_cat2%>%
  group_by(country_code)%>%
  filter(age_cat=="50-99")%>%
  select(country_code, country_label, age_cat, pAD, pAD_Lower, pAD_Upper)%>%
  rename("age2"="age_cat",
         "pAD_age2"="pAD",
         "pAD_age2_low"="pAD_Lower",
         "pAD_age2_upp"="pAD_Upper")%>%
  left_join(age_ss_low)%>%
  filter(pAD_age1<=pAD_age2)

age_ss_nonsf$country_label

  
  
#Seeing statistically significant differences between primary and secondary analysis

age_ss_secondary<-Avoidable_Deaths_age_cat2%>%
  group_by(country_code, age_cat)%>%
 # filter(age_cat=="Overall")%>%
  select(country_code, country_label, age_cat, 
         pAD, pAD_Lower, pAD_Upper, 
         pAD_max, pAD_Lower_max, pAD_Upper_max)%>%
  filter(pAD_Upper<=pAD_Lower_max)


age_ss_secondary


# Additional two tables (patient characteristics and net survival...)

#Table for patient characteristics of included patients + diagnosis years

additional_values<-bcan_SURV%>%
  mutate(country=replace(country,country=="Cote d'Ivoire", "C?te d'Ivoire"),
         country=replace(country,country=="France", "Martinique"))%>%
  left_join(country_codes, by = c("country"="country_label"))%>%
  group_by(country_code)%>%
  filter(age >= 15, age <= 99) %>%
  mutate(end_FU = max(year))%>%
  filter(year>=end_FU-4)%>% #Filtering so last follow up was within a five year period
  mutate(n_patients_total=n(),
         censored=case_when(
           surv_yytot<=5 & status=="Lost to follow-up" ~ 1, # just to count the number of patients lost to follow up 
           .default = 0
         ),
         censored_percent=round(sum(censored)/n_patients_total*100,2),
         count2=case_when(
           surv_yytot==0~ 1, # just to count the number of patients with zero survival time
           .default = 0),
         count3=sum(count2), # Count the number of zero survival times in diagnosis period
           
         count3_percent=round(count3/n_patients*100,2) # Compute the percent of zero survival times
         
         )%>%
  select(country_code, n_patients_total, censored_percent,  count3, count3_percent)%>%
  distinct()


table_patients<-bSURV%>%
  filter(age_cat=="Overall")%>%
  group_by(country)%>%
  mutate(censored=case_when(
                          surv_yytot<=5 & status=="Lost to follow-up" ~ 1, # just to count the number of patients lost to follow up 
                          .default = 0
  ))%>%
  mutate(min_year=min(year),
         max_year=max(year),
         n_patients=n(),
         n_events,
         percent_events=round(n_events/n_patients*100 ,2),
         age_down= quantile(age, 0.25) ,
         age_upp= quantile(age, 0.75),
         age_med= quantile(age, 0.5),
         surv_time_max=round(max(surv_yytot),2),
         st_down=round(quantile(surv_yytot, 0.25),2),
         st_upp=round(quantile(surv_yytot, 0.75),2),
         surv_time_med=round(median(surv_yytot)),2)%>%
  select(country_code,country, min_year, max_year, 
         n_patients, n_events,
         age_down, age_med, age_upp,percent_events,
         surv_time_max, surv_time_med, st_down, st_upp)%>%
  distinct()%>%
  left_join(additional_values,by=c("country_code"))%>%
  mutate(
  "Calender Period"= paste0( min_year, "-", max_year),
  "Included Patients, N" = n_patients,
  "Excluded Patients, N" = n_patients_total-n_patients,
  "Number of Events, N(%)" = paste0(n_events,"(", percent_events, ")"),
  "IQR of Age at Diagnosis (Years)"= paste0(age_med, "(", age_down, ", ", age_upp,")"),
  "IQR Survival Time after Diagnosis (Years)" =paste0(surv_time_med, "(", st_down, ", ", st_upp,")"),
  "Complete F-U" = 100-censored_percent,
  "Percent with zero survival time"=count3_percent
  #  select(country_code, n_patients_total, censored_percent,  count3, count3_percent)%>%
) %>%
  dplyr::rename("Country" = "country") %>%
  select(country_code,
    "Country",
    "Calender Period",
    "Included Patients, N" ,
    "Excluded Patients, N",
    "Number of Events, N(%)" ,
    "IQR of Age at Diagnosis (Years)",
    "IQR Survival Time after Diagnosis (Years)",
    "Complete F-U",
    "Percent with zero survival time"
  )



table_patients



write.csv2(table_patients, "\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\Breast Cancer\\Results\\table_patient_characteristics.csv", row.names = F)


#Net survival table 


NS_table<-NS_OS_PAF%>%
  mutate(Five_Year_all_cause_Surv=round(Five_Year_all_cause_Surv*100,2),
         OS_Lower_CI=round(OS_Lower_CI*100,2),
         OS_Upper_CI=round(OS_Upper_CI*100,2),
         
         Five_Year_Net_Surv=round(Five_Year_Net_Surv*100,2),
         NS_Lower_CI=round(NS_Lower_CI*100,2),
         NS_Upper_CI=round( NS_Upper_CI*100,2)
         )%>%
  mutate(  "Five Year Observed Survival (OS)"=paste0(Five_Year_all_cause_Surv,"(", OS_Lower_CI, ", ", OS_Upper_CI, ")"),
          "Five-Year Net Survival (NS)" =paste0(Five_Year_Net_Surv,"(",     NS_Lower_CI , ", ",    NS_Upper_CI, ")" ))%>%
  select("age_cat",  "country_code", "country_label", "Five Year Observed Survival (OS)", "Five-Year Net Survival (NS)")

write.csv2(NS_table, "\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\Oliver_Langselius\\Breast Cancer\\Results\\net_survival.csv", row.names = F)


