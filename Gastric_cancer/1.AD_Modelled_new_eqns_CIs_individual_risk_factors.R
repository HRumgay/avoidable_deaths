###############################################
#
# Net survival and Avoidable deaths - Gastric cancer
#
# UPDATED:
#   AD_treat_no_prev  treatable ignoring the preventable share
#   AD_prev_hp/obs/smk/ebv   preventable by single risk factor
#   risk_factor_overlap      exclusive exposure-pattern decomposition
#   AD_only_hp/obs/smk/ebv   preventable by ONE risk factor only (exclusive)
#   AD_multiple              preventable by two or more risk factors
#   AD_treat_extra           AD_treat_no_prev - AD_treat
#   -> number, proportion (pAD_*), ASR (*.asr) and GLOBOCAN UI for all of them
#
# NOTE on single-factor estimates: each uses that factor's PAF ALONE. Because
# the combined fraction is 1 - PROD(1 - PAF_i), the single-factor estimates do
# NOT sum to AD_prev -- cases with Interaction of two or more risk factors are counted once in the
# combined figure and once in each factor. Report them as individual
# attributable fractions, not as a partition. The `risk_factor_overlap` block
# gives the partition that DOES sum to the total.
#
# The exclusive pieces AD_only_hp + AD_only_obs + AD_only_smk + AD_only_ebv +
# AD_multiple DO sum to AD_prev (checked in section 3).
#
###############################################

# =============================================================================
# 0. PAFs -- carry each risk factor's attributable cases through
# =============================================================================
PAFs <- globocan%>%
  left_join(PAF_infections, by=c("country_code", "age", "sex"))%>%
  left_join(PAF_obesity_tobacco, by=c("country_code", "sex", "cancer_code"))%>%
  dplyr::mutate(age = case_when(
    age==0~ 0,
    age==1~ 1,
    age==2~ 2,
    age==3~ 3,
    age==4~ 4,
    age>4 & age<9 ~ 4,
    age==9 ~ 9,
    age==10~ 10,
    age==11~ 11,
    age==12~ 12,
    age==13~ 13,
    age==14~ 14,
    age==15~ 15,
    age==16 ~ 16,
    age==17 ~ 17,
    age==18 ~ 18,
  ))%>%
  as.data.frame()%>%
  dplyr::mutate(
    age_cat = case_when(
      age>=4 & age<14 ~ "15-64",
      age>=14 ~ "65-99",
      age<4 ~"0-15"
    ))%>%
  dplyr::filter(age_cat!="0-15")%>%
  droplevels()%>%
  as.data.frame()%>%
  group_by(country_code, sex, cancer_code, age)%>%
  filter(sex!=0)%>%
  as.data.frame()%>%
  mutate(af.comb=1-(1-OBS)*(1-SMK)*(1-af.hp)*(1-af.ebv), #infections
         cases.prev=cases*af.comb,
         cases.hp =cases*af.hp,      # single agent, overlap not removed
         cases.obs=cases*OBS,
         cases.smk=cases*SMK,
         cases.ebv=cases*af.ebv,
         cases.notprev=cases*(1-af.comb))%>%
  dplyr::select(country_label,country_code, cancer_label, cancer_code,
                sex, age_cat, age,
                cases.prev, cases.hp, cases.obs, cases.smk, cases.ebv,
                cases.notprev, cases, py,
                OBS, SMK, af.comb, af.hp, af.ebv)%>%
  ungroup()%>%
  distinct()%>%
  left_join(HDI_2022, by = c("country_code"))


# =============================================================================
# 0b. Exclusive exposure-pattern decomposition
#     Every case falls into exactly one region: one factor only, two or more,
#     or none. Products of (factor present) x (complements of factors absent)
#     -- no subtraction of pairwise or higher overlaps is needed, the product
#     form constructs the exclusive region directly.
#
#     ASSUMES the four exposures are independent in the population. They are
#     probably positively correlated in reality, so treat these as modelled
#     rather than observed overlap.
# =============================================================================
risk_factor_overlap <- PAFs %>%
  mutate(
    only_hp   = af.hp  * (1-OBS)   * (1-SMK) * (1-af.ebv),
    only_obs  = OBS    * (1-af.hp) * (1-SMK) * (1-af.ebv),
    only_smk  = SMK    * (1-af.hp) * (1-OBS) * (1-af.ebv),
    only_ebv  = af.ebv * (1-af.hp) * (1-OBS) * (1-SMK),
    exactly_one = only_hp + only_obs + only_smk + only_ebv,
    multiple    = af.comb - exactly_one,
    none        = 1 - af.comb
  ) %>%
  mutate(
    cases_only_hp  = cases * only_hp,
    cases_only_obs = cases * only_obs,
    cases_only_smk = cases * only_smk,
    cases_only_ebv = cases * only_ebv,
    cases_multiple = cases * multiple,
    cases_none     = cases * none
  )

# the six pieces must sum to total cases
risk_factor_overlap %>%
  summarise(check = sum(cases_only_hp + cases_only_obs + cases_only_smk +
                          cases_only_ebv + cases_multiple + cases_none,
                        na.rm = TRUE) -
              sum(cases, na.rm = TRUE))

# world summary of the partition, stomach only
risk_factor_overlap %>%
  filter(cancer_code == 7) %>%
  summarise(across(c(cases_only_hp, cases_only_obs, cases_only_smk,
                     cases_only_ebv, cases_multiple, cases_none, cases),
                   ~ sum(.x, na.rm = TRUE))) %>%
  mutate(across(starts_with("cases_"), ~ .x / cases * 100,
                .names = "pct_{.col}"))


# =============================================================================
# 1. Expected survival and relative survival
# =============================================================================
ES3 <- ES_expanded
ES2 <- ES3

countries_5y<-Survival_Modelled%>%
  filter(cancer_label!="Nasophar")%>%
  dplyr::mutate(rel_surv=case_when(rel_surv>1~ 1,
                                   rel_surv<=1~ rel_surv))%>%
  dplyr::mutate(country_label = str_remove( country_label,'"'))%>%
  dplyr::mutate(country_label = str_remove( country_label,"'"))%>%
  dplyr::mutate(country_label = str_remove( country_label,"`"))%>%
  dplyr::mutate(country_label = str_remove( country_label,'"'))%>%
  arrange(country_label)%>%
  distinct()

Countries_Simulated <-countries_5y%>%
  dplyr::mutate(age = case_when(
    age==0~ 0,
    age==1~ 1,
    age==2~ 2,
    age==3~ 3,
    age==4~ 4,
    age>4 & age<9 ~ 4,
    age==9 ~ 9,
    age==10~ 10,
    age==11~ 11,
    age==12~ 12,
    age==13~ 13,
    age==14~ 14,
    age==15~ 15,
    age==16 ~ 16,
    age==17 ~ 17,
    age==18 ~ 18,
  ))%>%
  dplyr::mutate(
    age_cat = case_when(
      age>=4 & age<14 ~ "15-64",
      age>=14 ~ "65-99",
      age<4 ~"0-15"
    ))%>%
  dplyr::filter(age_cat!="0-15")%>%
  dplyr::group_by(country_label,cancer_label,age_cat)%>%
  dplyr::select(country_code,country_label, cancer_code, cancer_label,
                age, age_cat, rel_surv)%>%
  as.data.frame()

simulated_overall<-Countries_Simulated%>%
  as.data.frame()%>%
  droplevels()%>%
  dplyr::group_by(country_label,cancer_label, age_cat,age)%>%
  dplyr::select(country_code,country_label, cancer_code, cancer_label,rel_surv,
                age_cat, age)%>%
  distinct()%>%
  arrange(country_label,cancer_label, age)%>%
  mutate(country_code = replace(country_code, country_code == 160, 156)) # China


# =============================================================================
# 2. PAF combinations -- all four af.* rebuilt from summed cases after the
#    age collapse (ages 5-8 fold into 4, so fractions must be recomputed
#    from summed numerators and denominators, never averaged)
# =============================================================================
PAFs_age_Cat <- PAFs%>%
  distinct()%>%
  as.data.frame()%>%
  dplyr::filter(age_cat!="0-15")%>%
  droplevels()%>%
  as.data.frame()%>%
  left_join(ES2, by=c("country_code","age","age_cat","sex"))%>%
  ungroup()%>%
  as.data.frame()%>%
  dplyr::select(country_code,country_label, hdi_group, cancer_code, cancer_label,
                age, sex, age_cat,
                cases, cases.prev, cases.hp, cases.obs, cases.smk, cases.ebv,
                cases.notprev, af.comb, af.hp,
                ES, py)%>%
  distinct()%>%
  group_by(country_label,cancer_label, age,sex) %>%
  distinct()%>%
  as.data.frame()%>%
  group_by(country_label,cancer_label, age, sex) %>%
  dplyr::mutate(country_code,country_label, cancer_code, cancer_label,
                age, sex,age_cat,
                cases=sum(cases, na.rm=T),
                cases.prev=sum(cases.prev, na.rm=T),
                cases.hp=sum(cases.hp, na.rm=T),
                cases.obs=sum(cases.obs, na.rm=T),
                cases.smk=sum(cases.smk, na.rm=T),
                cases.ebv=sum(cases.ebv, na.rm=T),
                cases.notprev=sum(cases.notprev, na.rm=T),
                total_overall=cases,
                py=sum(py),
                ES)%>%
  ungroup()%>%
  distinct()%>%
  group_by(country_label,cancer_label, age,sex) %>%
  as.data.frame()%>%
  dplyr::mutate(af.comb = case_when(cases!=0 ~ cases.prev/cases, cases==0 ~ 0),
                af.hp   = case_when(cases!=0 ~ cases.hp/cases,   cases==0 ~ 0),
                af.obs  = case_when(cases!=0 ~ cases.obs/cases,  cases==0 ~ 0),
                af.smk  = case_when(cases!=0 ~ cases.smk/cases,  cases==0 ~ 0),
                af.ebv  = case_when(cases!=0 ~ cases.ebv/cases,  cases==0 ~ 0))%>%
  distinct()%>%
  select(country_code,country_label, hdi_group, cancer_code, cancer_label,
         age, sex,age_cat,
         cases, cases.prev, cases.hp, cases.obs, cases.smk, cases.ebv,
         cases.notprev, af.comb, af.hp, af.obs, af.smk, af.ebv,
         total_overall, ES, py)%>%
  distinct()%>%
  dplyr::group_by(country_label,cancer_label, age)


PAFs2 <- PAFs_age_Cat%>%
  as.data.frame()%>%
  droplevels()%>%
  dplyr::group_by(country_code,cancer_code,cancer_label, age_cat,age,sex)%>%
  as.data.frame()%>%
  dplyr::group_by(country_code,cancer_code,age,sex)%>%
  dplyr::select(country_label, cancer_label, hdi_group,
                age_cat, age, total_overall, cases,
                cases.prev, cases.hp, cases.obs, cases.smk, cases.ebv,
                ES, af.comb, af.hp, af.obs, af.smk, af.ebv, py)%>%
  distinct()%>%
  arrange(cancer_label, age)%>%
  select(-cancer_label,-country_label)


Simulated_Data_PAF_1 <- simulated_overall%>%
  ungroup()%>%
  dplyr::filter(cancer_code==7)%>%
  dplyr::filter(age_cat!="Overall")%>%
  left_join(PAFs2,by=c("country_code"="country_code",
                       "cancer_code"="cancer_code",
                       "age_cat"="age_cat",
                       "age"="age"))%>%
  ungroup()%>%
  dplyr::group_by(country_code,cancer_code, age, sex)%>%
  as.data.frame()%>%
  dplyr::group_by(country_code,cancer_code, age, sex)%>%
  dplyr::select(country_code,
                country_label, hdi_group,
                cancer_code, cancer_label,
                age_cat, age, total_overall,
                rel_surv,
                af.comb, af.hp, af.obs, af.smk, af.ebv,
                ES,
                py)%>%
  droplevels()%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()


HDI_Region_Mapping2 <- HDI_Region_Mapping%>%
  dplyr::filter(area<=21)%>%
  dplyr::mutate(
    area = case_when(
      area %in% c(19,20,21)  ~ 19,
      !area %in% c(19,20,21)  ~ area
    ))%>%
  dplyr::mutate(
    country_label= case_when(
      area %in% c(19)  ~ "Melanesia/ Micronesia/ Polynesia ",
      !area %in% c(19)  ~ country_label
    ))%>%
  select(-country_label)%>%
  distinct()


Simulated_Data_PAF_All <- Simulated_Data_PAF_1%>%
  dplyr::mutate(rel_surv=as.double(rel_surv))%>%
  dplyr::mutate(across(c(af.comb, af.hp, af.obs, af.smk, af.ebv), as.double))%>%
  dplyr::mutate(total_overall=as.double(total_overall))%>%
  arrange(country_label,cancer_code,age,sex)%>%
  left_join(Reference_Survival, by=c("age","cancer_code"))%>%
  left_join(HDI_Region_Mapping2)


# =============================================================================
# 3. Avoidable deaths
#    AD_treat_no_prev : treatable ignoring the preventable share
#    AD_prev_*        : preventable attributable to one risk factor alone
#    AD_only_*        : preventable by that risk factor ONLY (exclusive)
#    AD_multiple      : preventable by two or more risk factors
#    AD_treat_extra   : AD_treat_no_prev - AD_treat
# =============================================================================
Avoidable_Deaths_Simulated_All3<-Simulated_Data_PAF_All%>%
  dplyr::group_by(country_code, cancer_code, age, sex)%>%
  dplyr::mutate(AD_prev= total_overall * af.comb *ES* (1 - rel_surv))%>%
  dplyr::mutate(AD_treat=(1-af.comb) *total_overall * (surv_ref-rel_surv) * ES)%>%
  dplyr::mutate(AD_treat_no_prev= total_overall * (surv_ref-rel_surv) * ES)%>%
  dplyr::mutate(AD_prev_hp  = total_overall * af.hp  * ES * (1 - rel_surv))%>%
  dplyr::mutate(AD_prev_obs = total_overall * af.obs * ES * (1 - rel_surv))%>%
  dplyr::mutate(AD_prev_smk = total_overall * af.smk * ES * (1 - rel_surv))%>%
  dplyr::mutate(AD_prev_ebv = total_overall * af.ebv * ES * (1 - rel_surv))%>%
  dplyr::mutate(AD_only_hp  = total_overall * af.hp  * (1-af.obs) * (1-af.smk) * (1-af.ebv) * ES * (1 - rel_surv))%>%
  dplyr::mutate(AD_only_obs = total_overall * af.obs * (1-af.hp)  * (1-af.smk) * (1-af.ebv) * ES * (1 - rel_surv))%>%
  dplyr::mutate(AD_only_smk = total_overall * af.smk * (1-af.hp)  * (1-af.obs) * (1-af.ebv) * ES * (1 - rel_surv))%>%
  dplyr::mutate(AD_only_ebv = total_overall * af.ebv * (1-af.hp)  * (1-af.obs) * (1-af.smk) * ES * (1 - rel_surv))%>%
  dplyr::mutate(AD_multiple = AD_prev - (AD_only_hp + AD_only_obs + AD_only_smk + AD_only_ebv))%>%
  dplyr::mutate(AD_unavoid =total_overall*(1-ES*surv_ref-af.comb*ES*(1-surv_ref)))%>%
  dplyr::mutate(total_deaths2=AD_prev+AD_treat+AD_unavoid)%>%
  select("country_code","country_label","age_cat","age","sex","cancer_code","cancer_label",
         "AD_treat",
         "AD_treat_no_prev",
         "AD_prev",
         "AD_prev_hp", "AD_prev_obs", "AD_prev_smk", "AD_prev_ebv",
         "AD_only_hp", "AD_only_obs", "AD_only_smk", "AD_only_ebv", "AD_multiple",
         "AD_unavoid",
         "total_deaths2",
         "total_overall",
         "hdi_group", py)%>%
  dplyr::rename("cancer"="cancer_label")


Avoidable_Deaths_Simulated_All2 <- Avoidable_Deaths_Simulated_All3%>%
  filter(cancer_code%in%c(7))


# ---- weights ----------------------------------------------------------------
w2 <- c(12000,10000,9000,9000,8000,8000,6000,6000,6000,6000,5000,4000,4000,3000,2000,1000,500,500)
age <- c(1:18)
weights2 <- data.frame(w2, age)

weights2 <- weights2%>%dplyr::mutate(age = case_when(
  age==1~ 1, age==2~ 2, age==3~ 3, age==4~ 4,
  age>4 & age<9 ~ 4,
  age==9 ~ 9, age==10 ~ 10, age==11 ~ 11, age==12 ~ 12,
  age==13 ~ 13, age==14 ~ 14, age==15 ~ 15,
  age==16 ~ 16, age==17 ~ 17, age==18 ~ 18))%>%
  dplyr::filter(age>3)%>%
  dplyr::mutate(total=sum(w2))%>%
  dplyr::group_by(age)%>%
  dplyr::mutate(w=sum(w2)/total)%>%
  select(-w2, -total)%>%
  distinct()


AD_FACTORS <- c("AD_prev_hp", "AD_prev_obs", "AD_prev_smk", "AD_prev_ebv")
AD_EXCL    <- c("AD_only_hp", "AD_only_obs", "AD_only_smk", "AD_only_ebv", "AD_multiple")

Avoidable_Deaths_Simulated_All<- Avoidable_Deaths_Simulated_All2%>%
  as.data.frame()%>%
  dplyr::mutate(age=as.numeric(as.character(age)))%>%
  dplyr::mutate(across(c(AD_prev, AD_treat, AD_treat_no_prev, AD_unavoid,
                         total_overall, all_of(AD_FACTORS)),
                       ~ as.numeric(as.character(.x))))%>%
  dplyr::mutate(across(all_of(AD_EXCL), ~ as.numeric(as.character(.x))))%>%
  dplyr::mutate(AD_sum=AD_prev + AD_unavoid + AD_treat)%>%
  dplyr::mutate(cancer_code=as.numeric(cancer_code))%>%
  as.data.frame()%>%distinct()%>%
  dplyr::mutate(country_code=as.integer(country_code))%>%
  left_join(weights2)%>%
  dplyr::mutate(AD_treat = case_when(AD_treat<0 ~ 0,
                                     TRUE ~ AD_treat))%>%
  dplyr::mutate(AD_treat_no_prev = case_when(AD_treat_no_prev<0 ~ 0,
                                             TRUE ~ AD_treat_no_prev))%>%
  dplyr::mutate(AD_treat_extra = AD_treat_no_prev - AD_treat)

Avoidable_Deaths_Simulated_All_overall<-Avoidable_Deaths_Simulated_All%>%
  mutate(sex=0)%>%
  dplyr::group_by(country_code, cancer_code, sex, age)%>%
  dplyr::mutate(AD_prev=sum(AD_prev),
                AD_prev_hp=sum(AD_prev_hp),
                AD_prev_obs=sum(AD_prev_obs),
                AD_prev_smk=sum(AD_prev_smk),
                AD_prev_ebv=sum(AD_prev_ebv),
                AD_only_hp=sum(AD_only_hp),
                AD_only_obs=sum(AD_only_obs),
                AD_only_smk=sum(AD_only_smk),
                AD_only_ebv=sum(AD_only_ebv),
                AD_multiple=sum(AD_multiple),
                AD_treat=sum(AD_treat),
                AD_treat_no_prev=sum(AD_treat_no_prev),
                AD_treat_extra=sum(AD_treat_extra),
                AD_unavoid=sum(AD_unavoid),
                total_deaths2=sum(total_deaths2),
                total_overall=sum(total_overall),
                py=sum(py),
                AD_sum=sum(AD_sum))%>%
  distinct()%>%
  as.data.frame()

Avoidable_Deaths_Simulated_All<-Avoidable_Deaths_Simulated_All%>%
  full_join(Avoidable_Deaths_Simulated_All_overall)


# ---- checks -----------------------------------------------------------------
# every single-factor estimate must be <= AD_prev (its af is one component)
# AD_treat_no_prev must be >= AD_treat (it drops the (1-af.comb) factor)
Avoidable_Deaths_Simulated_All %>%
  summarise(hp_ok    = all(AD_prev_hp  <= AD_prev + 1e-6, na.rm=TRUE),
            obs_ok   = all(AD_prev_obs <= AD_prev + 1e-6, na.rm=TRUE),
            smk_ok   = all(AD_prev_smk <= AD_prev + 1e-6, na.rm=TRUE),
            ebv_ok   = all(AD_prev_ebv <= AD_prev + 1e-6, na.rm=TRUE),
            treat_ok = all(AD_treat_no_prev >= AD_treat - 1e-6, na.rm=TRUE))

# the five exclusive pieces must sum to AD_prev, and AD_treat_extra >= 0
Avoidable_Deaths_Simulated_All %>%
  summarise(excl_ok  = all(abs(AD_only_hp + AD_only_obs + AD_only_smk +
                                 AD_only_ebv + AD_multiple - AD_prev) < 1e-6, na.rm=TRUE),
            extra_ok = all(AD_treat_extra >= -1e-6, na.rm=TRUE))

# the four single-factor estimates SHOULD exceed AD_prev when summed -- that
# excess is the overlap, counted once per factor it belongs to
Avoidable_Deaths_Simulated_All %>%
  filter(sex != 0) %>%
  summarise(AD_prev      = sum(AD_prev, na.rm=TRUE),
            sum_factors  = sum(AD_prev_hp + AD_prev_obs +
                                 AD_prev_smk + AD_prev_ebv, na.rm=TRUE)) %>%
  mutate(overlap_excess = sum_factors - AD_prev,
         pct_excess     = overlap_excess / AD_prev * 100)

sum(Avoidable_Deaths_Simulated_All$total_deaths2[
  Avoidable_Deaths_Simulated_All$sex!=0])


# =============================================================================
# 4. Overall and by age category
# =============================================================================
Avoidable_Deaths_Simulated_All_overall <- Avoidable_Deaths_Simulated_All%>%
  dplyr::mutate(age_cat="Overall")%>%
  dplyr::group_by(country_code, cancer_code, sex, age_cat)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w),
                AD_prev_hp.asr=sum(AD_prev_hp/py*100000*w),
                AD_prev_obs.asr=sum(AD_prev_obs/py*100000*w),
                AD_prev_smk.asr=sum(AD_prev_smk/py*100000*w),
                AD_prev_ebv.asr=sum(AD_prev_ebv/py*100000*w),
                AD_only_hp.asr=sum(AD_only_hp/py*100000*w),
                AD_only_obs.asr=sum(AD_only_obs/py*100000*w),
                AD_only_smk.asr=sum(AD_only_smk/py*100000*w),
                AD_only_ebv.asr=sum(AD_only_ebv/py*100000*w),
                AD_multiple.asr=sum(AD_multiple/py*100000*w),
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_no_prev.asr=sum(AD_treat_no_prev/py*100000*w),
                AD_treat_extra.asr=sum(AD_treat_extra/py*100000*w),
                AD_sum.asr=sum(AD_sum/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  select( -total_overall)%>%
  dplyr::mutate(AD_prev = sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(across(all_of(AD_FACTORS), ~ sum(.x, na.rm=T)))%>%
  dplyr::mutate(across(all_of(AD_EXCL), ~ sum(.x, na.rm=T)))%>%
  dplyr::mutate(AD_treat = sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_treat_no_prev = sum(AD_treat_no_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat_extra = sum(AD_treat_extra,na.rm=T))%>%
  dplyr::mutate(AD_sum = sum(AD_sum,na.rm=T))%>%
  dplyr::mutate(AD_unavoid = sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2,na.rm=T))%>%
  dplyr::mutate(py=sum(py))%>%
  select(-age)%>%
  ungroup()%>%
  dplyr::group_by(cancer_code, country_code, sex, age_cat)%>%
  dplyr::mutate(w=sum(w/2))%>%
  distinct()%>%
  as.data.frame()


Avoidable_Deaths_Simulated_All_age_cat <- Avoidable_Deaths_Simulated_All%>%
  ungroup()%>%
  dplyr::group_by(country_code, cancer_code,sex,  age_cat)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w),
                AD_prev_hp.asr=sum(AD_prev_hp/py*100000*w),
                AD_prev_obs.asr=sum(AD_prev_obs/py*100000*w),
                AD_prev_smk.asr=sum(AD_prev_smk/py*100000*w),
                AD_prev_ebv.asr=sum(AD_prev_ebv/py*100000*w),
                AD_only_hp.asr=sum(AD_only_hp/py*100000*w),
                AD_only_obs.asr=sum(AD_only_obs/py*100000*w),
                AD_only_smk.asr=sum(AD_only_smk/py*100000*w),
                AD_only_ebv.asr=sum(AD_only_ebv/py*100000*w),
                AD_multiple.asr=sum(AD_multiple/py*100000*w),
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_no_prev.asr=sum(AD_treat_no_prev/py*100000*w),
                AD_treat_extra.asr=sum(AD_treat_extra/py*100000*w),
                AD_sum.asr=sum(AD_sum/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  select(-total_overall)%>%
  dplyr::mutate(AD_prev = sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(across(all_of(AD_FACTORS), ~ sum(.x, na.rm=T)))%>%
  dplyr::mutate(across(all_of(AD_EXCL), ~ sum(.x, na.rm=T)))%>%
  dplyr::mutate(AD_treat = sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_treat_no_prev = sum(AD_treat_no_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat_extra = sum(AD_treat_extra,na.rm=T))%>%
  dplyr::mutate(AD_unavoid = sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2,na.rm=T))%>%
  dplyr::mutate(AD_sum = sum(AD_sum,na.rm=T))%>%
  dplyr::mutate(py=sum(py))%>%
  select( -age)%>%
  ungroup()%>%
  dplyr::group_by(cancer_code, country_code, sex, age_cat)%>%
  dplyr::mutate(w=sum(w/2))%>%
  distinct()%>%
  full_join(Avoidable_Deaths_Simulated_All_overall)%>%
  as.data.frame()%>%
  ungroup()


# =============================================================================
# 5. By HDI
# =============================================================================
AD_by_HDI <- Avoidable_Deaths_Simulated_All%>%
  dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::group_by(hdi_group, cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  dplyr::mutate(AD_treat_prev = AD_prev + AD_treat)%>%
  select(-country_label,-country_code,  hdi_group, cancer, cancer_code,
         AD_treat, AD_treat_no_prev, AD_prev,
         AD_prev_hp, AD_prev_obs, AD_prev_smk, AD_prev_ebv,
         AD_only_hp, AD_only_obs, AD_only_smk, AD_only_ebv, AD_multiple,
         AD_treat_extra,
         AD_unavoid, AD_sum, -age, age_cat,-total_overall)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%
  dplyr::group_by(hdi_group,sex, cancer)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w),
                AD_prev_hp.asr=sum(AD_prev_hp/py*100000*w),
                AD_prev_obs.asr=sum(AD_prev_obs/py*100000*w),
                AD_prev_smk.asr=sum(AD_prev_smk/py*100000*w),
                AD_prev_ebv.asr=sum(AD_prev_ebv/py*100000*w),
                AD_only_hp.asr=sum(AD_only_hp/py*100000*w),
                AD_only_obs.asr=sum(AD_only_obs/py*100000*w),
                AD_only_smk.asr=sum(AD_only_smk/py*100000*w),
                AD_only_ebv.asr=sum(AD_only_ebv/py*100000*w),
                AD_multiple.asr=sum(AD_multiple/py*100000*w),
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_no_prev.asr=sum(AD_treat_no_prev/py*100000*w),
                AD_treat_extra.asr=sum(AD_treat_extra/py*100000*w),
                AD_sum.asr=sum(AD_sum/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  dplyr::mutate(AD_treat_prev=sum(AD_prev+AD_treat))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2))%>%
  dplyr::mutate(AD_treat=sum(AD_treat))%>%
  dplyr::mutate(AD_treat_no_prev=sum(AD_treat_no_prev))%>%
  dplyr::mutate(AD_treat_extra=sum(AD_treat_extra))%>%
  dplyr::mutate(AD_prev=sum(AD_prev))%>%
  dplyr::mutate(across(all_of(AD_FACTORS), sum))%>%
  dplyr::mutate(across(all_of(AD_EXCL), sum))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid))%>%
  dplyr::mutate(total_deaths=total_deaths2)%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_treat_no_prev=AD_treat_no_prev/total_deaths)%>%
  dplyr::mutate(pAD_treat_extra=AD_treat_extra/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev_hp=AD_prev_hp/total_deaths)%>%
  dplyr::mutate(pAD_prev_obs=AD_prev_obs/total_deaths)%>%
  dplyr::mutate(pAD_prev_smk=AD_prev_smk/total_deaths)%>%
  dplyr::mutate(pAD_prev_ebv=AD_prev_ebv/total_deaths)%>%
  dplyr::mutate(pAD_only_hp=AD_only_hp/total_deaths)%>%
  dplyr::mutate(pAD_only_obs=AD_only_obs/total_deaths)%>%
  dplyr::mutate(pAD_only_smk=AD_only_smk/total_deaths)%>%
  dplyr::mutate(pAD_only_ebv=AD_only_ebv/total_deaths)%>%
  dplyr::mutate(pAD_multiple=AD_multiple/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  dplyr::mutate(w=sum(w)/n())%>%
  select(-AD_sum,-py)%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()


AD_by_HDI_all <-Avoidable_Deaths_Simulated_All%>%
  dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::group_by(hdi_group, cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  select(-country_label,-country_code,-cancer_code, -cancer,  hdi_group,
         AD_treat, AD_treat_no_prev, AD_prev,
         AD_prev_hp, AD_prev_obs, AD_prev_smk, AD_prev_ebv,
         AD_only_hp, AD_only_obs, AD_only_smk, AD_only_ebv, AD_multiple,
         AD_treat_extra,
         AD_unavoid, total_deaths2, AD_sum, -age, age_cat,-total_overall)%>%
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
  dplyr::group_by(hdi_group, sex)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w),
                AD_prev_hp.asr=sum(AD_prev_hp/py*100000*w),
                AD_prev_obs.asr=sum(AD_prev_obs/py*100000*w),
                AD_prev_smk.asr=sum(AD_prev_smk/py*100000*w),
                AD_prev_ebv.asr=sum(AD_prev_ebv/py*100000*w),
                AD_only_hp.asr=sum(AD_only_hp/py*100000*w),
                AD_only_obs.asr=sum(AD_only_obs/py*100000*w),
                AD_only_smk.asr=sum(AD_only_smk/py*100000*w),
                AD_only_ebv.asr=sum(AD_only_ebv/py*100000*w),
                AD_multiple.asr=sum(AD_multiple/py*100000*w),
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_no_prev.asr=sum(AD_treat_no_prev/py*100000*w),
                AD_treat_extra.asr=sum(AD_treat_extra/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
                AD_sum.asr=sum(AD_sum/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  dplyr::mutate(AD_treat_prev=sum(AD_prev+AD_treat))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2))%>%
  dplyr::mutate(AD_treat=sum(AD_treat))%>%
  dplyr::mutate(AD_treat_no_prev=sum(AD_treat_no_prev))%>%
  dplyr::mutate(AD_treat_extra=sum(AD_treat_extra))%>%
  dplyr::mutate(AD_prev=sum(AD_prev))%>%
  dplyr::mutate(across(all_of(AD_FACTORS), sum))%>%
  dplyr::mutate(across(all_of(AD_EXCL), sum))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid))%>%
  dplyr::mutate(total_deaths=total_deaths2)%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_treat_no_prev=AD_treat_no_prev/total_deaths)%>%
  dplyr::mutate(pAD_treat_extra=AD_treat_extra/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev_hp=AD_prev_hp/total_deaths)%>%
  dplyr::mutate(pAD_prev_obs=AD_prev_obs/total_deaths)%>%
  dplyr::mutate(pAD_prev_smk=AD_prev_smk/total_deaths)%>%
  dplyr::mutate(pAD_prev_ebv=AD_prev_ebv/total_deaths)%>%
  dplyr::mutate(pAD_only_hp=AD_only_hp/total_deaths)%>%
  dplyr::mutate(pAD_only_obs=AD_only_obs/total_deaths)%>%
  dplyr::mutate(pAD_only_smk=AD_only_smk/total_deaths)%>%
  dplyr::mutate(pAD_only_ebv=AD_only_ebv/total_deaths)%>%
  dplyr::mutate(pAD_multiple=AD_multiple/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  dplyr::mutate(cancer="All Cancer Sites")%>%
  dplyr::mutate(w=sum(w)/n())%>%
  select(-AD_sum,-py)%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()%>%
  dplyr::rename("country_code"="hdi_group")


AD_by_HDI_all_cancer <-Avoidable_Deaths_Simulated_All%>%
  dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::group_by(hdi_group, cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  select(-country_label,-country_code,cancer_code, cancer, hdi_group,
         AD_treat, AD_treat_no_prev, AD_prev,
         AD_prev_hp, AD_prev_obs, AD_prev_smk, AD_prev_ebv,
         AD_only_hp, AD_only_obs, AD_only_smk, AD_only_ebv, AD_multiple,
         AD_treat_extra,
         AD_unavoid, total_deaths2, AD_sum, -age, age_cat,-total_overall)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  dplyr::mutate(hdi_group=case_when(
    hdi_group==1~ 984,
    hdi_group==2~ 983,
    hdi_group==3~ 982,
    hdi_group==4~ 981
  ))%>%
  ungroup()%>%
  dplyr::group_by(hdi_group, sex, cancer)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w),
                AD_prev_hp.asr=sum(AD_prev_hp/py*100000*w),
                AD_prev_obs.asr=sum(AD_prev_obs/py*100000*w),
                AD_prev_smk.asr=sum(AD_prev_smk/py*100000*w),
                AD_prev_ebv.asr=sum(AD_prev_ebv/py*100000*w),
                AD_only_hp.asr=sum(AD_only_hp/py*100000*w),
                AD_only_obs.asr=sum(AD_only_obs/py*100000*w),
                AD_only_smk.asr=sum(AD_only_smk/py*100000*w),
                AD_only_ebv.asr=sum(AD_only_ebv/py*100000*w),
                AD_multiple.asr=sum(AD_multiple/py*100000*w),
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_no_prev.asr=sum(AD_treat_no_prev/py*100000*w),
                AD_treat_extra.asr=sum(AD_treat_extra/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
                AD_sum.asr=sum(AD_sum/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  dplyr::mutate(AD_treat_prev=sum(AD_prev+AD_treat))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2))%>%
  dplyr::mutate(AD_treat=sum(AD_treat))%>%
  dplyr::mutate(AD_treat_no_prev=sum(AD_treat_no_prev))%>%
  dplyr::mutate(AD_treat_extra=sum(AD_treat_extra))%>%
  dplyr::mutate(AD_prev=sum(AD_prev))%>%
  dplyr::mutate(across(all_of(AD_FACTORS), sum))%>%
  dplyr::mutate(across(all_of(AD_EXCL), sum))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid))%>%
  dplyr::mutate(total_deaths=total_deaths2)%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_treat_no_prev=AD_treat_no_prev/total_deaths)%>%
  dplyr::mutate(pAD_treat_extra=AD_treat_extra/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev_hp=AD_prev_hp/total_deaths)%>%
  dplyr::mutate(pAD_prev_obs=AD_prev_obs/total_deaths)%>%
  dplyr::mutate(pAD_prev_smk=AD_prev_smk/total_deaths)%>%
  dplyr::mutate(pAD_prev_ebv=AD_prev_ebv/total_deaths)%>%
  dplyr::mutate(pAD_only_hp=AD_only_hp/total_deaths)%>%
  dplyr::mutate(pAD_only_obs=AD_only_obs/total_deaths)%>%
  dplyr::mutate(pAD_only_smk=AD_only_smk/total_deaths)%>%
  dplyr::mutate(pAD_only_ebv=AD_only_ebv/total_deaths)%>%
  dplyr::mutate(pAD_multiple=AD_multiple/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  dplyr::mutate(w=sum(w)/n())%>%
  select(-AD_sum,-py)%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()%>%
  dplyr::rename("country_code"="hdi_group")


# =============================================================================
# 6. By country
# =============================================================================
AD_country_all_cancers <-Avoidable_Deaths_Simulated_All%>%
  dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::group_by(country_label, cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  select(-age,-total_overall,-AD_sum)%>%
  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(  -hdi_group,  )%>%
  dplyr::group_by(country_code, sex)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w),
                AD_prev_hp.asr=sum(AD_prev_hp/py*100000*w),
                AD_prev_obs.asr=sum(AD_prev_obs/py*100000*w),
                AD_prev_smk.asr=sum(AD_prev_smk/py*100000*w),
                AD_prev_ebv.asr=sum(AD_prev_ebv/py*100000*w),
                AD_only_hp.asr=sum(AD_only_hp/py*100000*w),
                AD_only_obs.asr=sum(AD_only_obs/py*100000*w),
                AD_only_smk.asr=sum(AD_only_smk/py*100000*w),
                AD_only_ebv.asr=sum(AD_only_ebv/py*100000*w),
                AD_multiple.asr=sum(AD_multiple/py*100000*w),
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_no_prev.asr=sum(AD_treat_no_prev/py*100000*w),
                AD_treat_extra.asr=sum(AD_treat_extra/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  dplyr::mutate(total_deaths=sum(AD_prev, AD_unavoid, AD_treat,na.rm=T))%>%
  dplyr::mutate(total_deaths2=sum(total_deaths2,na.rm=T))%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_treat_no_prev=sum(AD_treat_no_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat_extra=sum(AD_treat_extra,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(across(all_of(AD_FACTORS), ~ sum(.x, na.rm=T)))%>%
  dplyr::mutate(across(all_of(AD_EXCL), ~ sum(.x, na.rm=T)))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_treat_no_prev=AD_treat_no_prev/total_deaths)%>%
  dplyr::mutate(pAD_treat_extra=AD_treat_extra/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev_hp=AD_prev_hp/total_deaths)%>%
  dplyr::mutate(pAD_prev_obs=AD_prev_obs/total_deaths)%>%
  dplyr::mutate(pAD_prev_smk=AD_prev_smk/total_deaths)%>%
  dplyr::mutate(pAD_prev_ebv=AD_prev_ebv/total_deaths)%>%
  dplyr::mutate(pAD_only_hp=AD_only_hp/total_deaths)%>%
  dplyr::mutate(pAD_only_obs=AD_only_obs/total_deaths)%>%
  dplyr::mutate(pAD_only_smk=AD_only_smk/total_deaths)%>%
  dplyr::mutate(pAD_only_ebv=AD_only_ebv/total_deaths)%>%
  dplyr::mutate(pAD_multiple=AD_multiple/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  dplyr::mutate(cancer="All Cancer Sites")%>%
  dplyr::mutate(cancer_code=40)%>%
  select(-py,-w)%>%
  distinct()%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()


# =============================================================================
# 7. By cancer site (world total)
# =============================================================================
AD_cancer <- Avoidable_Deaths_Simulated_All%>%
  dplyr::mutate(age_cat="Overall")%>%
  dplyr::group_by(cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(-hdi_group,  -AD_sum,-age)%>%
  dplyr::mutate(country_code=900)%>%
  dplyr::mutate(country_label="All Countries")%>%
  dplyr::group_by(cancer, sex)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w),
                AD_prev_hp.asr=sum(AD_prev_hp/py*100000*w),
                AD_prev_obs.asr=sum(AD_prev_obs/py*100000*w),
                AD_prev_smk.asr=sum(AD_prev_smk/py*100000*w),
                AD_prev_ebv.asr=sum(AD_prev_ebv/py*100000*w),
                AD_only_hp.asr=sum(AD_only_hp/py*100000*w),
                AD_only_obs.asr=sum(AD_only_obs/py*100000*w),
                AD_only_smk.asr=sum(AD_only_smk/py*100000*w),
                AD_only_ebv.asr=sum(AD_only_ebv/py*100000*w),
                AD_multiple.asr=sum(AD_multiple/py*100000*w),
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_no_prev.asr=sum(AD_treat_no_prev/py*100000*w),
                AD_treat_extra.asr=sum(AD_treat_extra/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  select(-total_deaths2)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_treat_no_prev=sum(AD_treat_no_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat_extra=sum(AD_treat_extra,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(across(all_of(AD_FACTORS), ~ sum(.x, na.rm=T)))%>%
  dplyr::mutate(across(all_of(AD_EXCL), ~ sum(.x, na.rm=T)))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_treat_no_prev=AD_treat_no_prev/total_deaths)%>%
  dplyr::mutate(pAD_treat_extra=AD_treat_extra/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev_hp=AD_prev_hp/total_deaths)%>%
  dplyr::mutate(pAD_prev_obs=AD_prev_obs/total_deaths)%>%
  dplyr::mutate(pAD_prev_smk=AD_prev_smk/total_deaths)%>%
  dplyr::mutate(pAD_prev_ebv=AD_prev_ebv/total_deaths)%>%
  dplyr::mutate(pAD_only_hp=AD_only_hp/total_deaths)%>%
  dplyr::mutate(pAD_only_obs=AD_only_obs/total_deaths)%>%
  dplyr::mutate(pAD_only_smk=AD_only_smk/total_deaths)%>%
  dplyr::mutate(pAD_only_ebv=AD_only_ebv/total_deaths)%>%
  dplyr::mutate(pAD_multiple=AD_multiple/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  select(-py,-w,-total_overall)%>%
  distinct()%>%
  ungroup()%>%
  distinct()%>%
  as.data.frame()


# =============================================================================
# 8. By region
# =============================================================================
areas <- HDI_Region_Mapping%>%
  dplyr::filter(country_code>=910& country_code<=931 |
                  country_code==905 | country_code==906|
                  country_code==954| country_code==957 )%>%
  select(area, country_label)%>%
  dplyr::mutate( area = case_when(
    area %in% c(19,20,21)  ~ 19,
    !area %in% c(19,20,21)  ~ area
  ))%>%
  dplyr::mutate(
    country_label= case_when(
      area %in% c(19)  ~ "Melanesia/ Micronesia/ Polynesia ",
      !area %in% c(19)  ~ country_label
    ))%>%
  distinct()


AD_Region2 <- Avoidable_Deaths_Simulated_All%>%
  left_join(HDI_Region_Mapping2, by=c("country_code"))%>%
  dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::group_by(area, cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  select(-country_label,-country_code,-cancer_code, -cancer,  hdi_group,
         AD_treat, AD_treat_no_prev, AD_prev,
         AD_prev_hp, AD_prev_obs, AD_prev_smk, AD_prev_ebv,
         AD_only_hp, AD_only_obs, AD_only_smk, AD_only_ebv, AD_multiple,
         AD_treat_extra,
         AD_unavoid, total_deaths2, -age, age_cat,-total_overall)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(-hdi_group,-continent,   -AD_sum)%>%
  dplyr::group_by(area, sex)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w),
                AD_prev_hp.asr=sum(AD_prev_hp/py*100000*w),
                AD_prev_obs.asr=sum(AD_prev_obs/py*100000*w),
                AD_prev_smk.asr=sum(AD_prev_smk/py*100000*w),
                AD_prev_ebv.asr=sum(AD_prev_ebv/py*100000*w),
                AD_only_hp.asr=sum(AD_only_hp/py*100000*w),
                AD_only_obs.asr=sum(AD_only_obs/py*100000*w),
                AD_only_smk.asr=sum(AD_only_smk/py*100000*w),
                AD_only_ebv.asr=sum(AD_only_ebv/py*100000*w),
                AD_multiple.asr=sum(AD_multiple/py*100000*w),
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_no_prev.asr=sum(AD_treat_no_prev/py*100000*w),
                AD_treat_extra.asr=sum(AD_treat_extra/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  select(-total_deaths2)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_treat_no_prev=sum(AD_treat_no_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat_extra=sum(AD_treat_extra,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(across(all_of(AD_FACTORS), ~ sum(.x, na.rm=T)))%>%
  dplyr::mutate(across(all_of(AD_EXCL), ~ sum(.x, na.rm=T)))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_treat_no_prev=AD_treat_no_prev/total_deaths)%>%
  dplyr::mutate(pAD_treat_extra=AD_treat_extra/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev_hp=AD_prev_hp/total_deaths)%>%
  dplyr::mutate(pAD_prev_obs=AD_prev_obs/total_deaths)%>%
  dplyr::mutate(pAD_prev_smk=AD_prev_smk/total_deaths)%>%
  dplyr::mutate(pAD_prev_ebv=AD_prev_ebv/total_deaths)%>%
  dplyr::mutate(pAD_only_hp=AD_only_hp/total_deaths)%>%
  dplyr::mutate(pAD_only_obs=AD_only_obs/total_deaths)%>%
  dplyr::mutate(pAD_only_smk=AD_only_smk/total_deaths)%>%
  dplyr::mutate(pAD_only_ebv=AD_only_ebv/total_deaths)%>%
  dplyr::mutate(pAD_multiple=AD_multiple/total_deaths)%>%
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


AD_Region_cancer_sites <- Avoidable_Deaths_Simulated_All%>%
  left_join(HDI_Region_Mapping2, by=c("country_code"))%>%
  dplyr::mutate(age_cat="Overall")%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::group_by(area, cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  select(-country_label,-country_code,-cancer_code, hdi_group,
         AD_treat, AD_treat_no_prev, AD_prev,
         AD_prev_hp, AD_prev_obs, AD_prev_smk, AD_prev_ebv,
         AD_only_hp, AD_only_obs, AD_only_smk, AD_only_ebv, AD_multiple,
         AD_treat_extra,
         AD_unavoid, total_deaths2, -age, age_cat,-total_overall)%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  select(-hdi_group,-continent,   -AD_sum)%>%
  dplyr::group_by(area,cancer, sex)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w),
                AD_prev_hp.asr=sum(AD_prev_hp/py*100000*w),
                AD_prev_obs.asr=sum(AD_prev_obs/py*100000*w),
                AD_prev_smk.asr=sum(AD_prev_smk/py*100000*w),
                AD_prev_ebv.asr=sum(AD_prev_ebv/py*100000*w),
                AD_only_hp.asr=sum(AD_only_hp/py*100000*w),
                AD_only_obs.asr=sum(AD_only_obs/py*100000*w),
                AD_only_smk.asr=sum(AD_only_smk/py*100000*w),
                AD_only_ebv.asr=sum(AD_only_ebv/py*100000*w),
                AD_multiple.asr=sum(AD_multiple/py*100000*w),
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_no_prev.asr=sum(AD_treat_no_prev/py*100000*w),
                AD_treat_extra.asr=sum(AD_treat_extra/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  select(-total_deaths2)%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_treat_no_prev=sum(AD_treat_no_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat_extra=sum(AD_treat_extra,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(across(all_of(AD_FACTORS), ~ sum(.x, na.rm=T)))%>%
  dplyr::mutate(across(all_of(AD_EXCL), ~ sum(.x, na.rm=T)))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_treat_no_prev=AD_treat_no_prev/total_deaths)%>%
  dplyr::mutate(pAD_treat_extra=AD_treat_extra/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev_hp=AD_prev_hp/total_deaths)%>%
  dplyr::mutate(pAD_prev_obs=AD_prev_obs/total_deaths)%>%
  dplyr::mutate(pAD_prev_smk=AD_prev_smk/total_deaths)%>%
  dplyr::mutate(pAD_prev_ebv=AD_prev_ebv/total_deaths)%>%
  dplyr::mutate(pAD_only_hp=AD_only_hp/total_deaths)%>%
  dplyr::mutate(pAD_only_obs=AD_only_obs/total_deaths)%>%
  dplyr::mutate(pAD_only_smk=AD_only_smk/total_deaths)%>%
  dplyr::mutate(pAD_only_ebv=AD_only_ebv/total_deaths)%>%
  dplyr::mutate(pAD_multiple=AD_multiple/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  select(-py,-w)%>%
  distinct()%>%
  left_join(areas, by=c("area"))%>%
  ungroup()%>%
  distinct()%>%
  left_join(HDI_Region_Mapping, by=c("area", "country_label"))%>%
  as.data.frame()%>%
  dplyr::group_by(area)


countries_regions<-Avoidable_Deaths_Simulated_All%>%
  select(country_code)%>%
  distinct()%>%
  left_join(HDI_Region_Mapping2, by=c("country_code"))


# =============================================================================
# 9. Table 1
# =============================================================================
table_1_1 <- Avoidable_Deaths_Simulated_All %>%
  select(-country_label, -country_code, -cancer_code, hdi_group)%>%
  dplyr::mutate(age_cat="Overall")%>%
  dplyr::group_by(cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%
  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  group_by(sex)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w),
                AD_prev_hp.asr=sum(AD_prev_hp/py*100000*w),
                AD_prev_obs.asr=sum(AD_prev_obs/py*100000*w),
                AD_prev_smk.asr=sum(AD_prev_smk/py*100000*w),
                AD_prev_ebv.asr=sum(AD_prev_ebv/py*100000*w),
                AD_only_hp.asr=sum(AD_only_hp/py*100000*w),
                AD_only_obs.asr=sum(AD_only_obs/py*100000*w),
                AD_only_smk.asr=sum(AD_only_smk/py*100000*w),
                AD_only_ebv.asr=sum(AD_only_ebv/py*100000*w),
                AD_multiple.asr=sum(AD_multiple/py*100000*w),
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_no_prev.asr=sum(AD_treat_no_prev/py*100000*w),
                AD_treat_extra.asr=sum(AD_treat_extra/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat+AD_prev)/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_treat_no_prev=sum(AD_treat_no_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat_extra=sum(AD_treat_extra,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(across(all_of(AD_FACTORS), ~ sum(.x, na.rm=T)))%>%
  dplyr::mutate(across(all_of(AD_EXCL), ~ sum(.x, na.rm=T)))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_treat_no_prev=AD_treat_no_prev/total_deaths)%>%
  dplyr::mutate(pAD_treat_extra=AD_treat_extra/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev_hp=AD_prev_hp/total_deaths)%>%
  dplyr::mutate(pAD_prev_obs=AD_prev_obs/total_deaths)%>%
  dplyr::mutate(pAD_prev_smk=AD_prev_smk/total_deaths)%>%
  dplyr::mutate(pAD_prev_ebv=AD_prev_ebv/total_deaths)%>%
  dplyr::mutate(pAD_only_hp=AD_only_hp/total_deaths)%>%
  dplyr::mutate(pAD_only_obs=AD_only_obs/total_deaths)%>%
  dplyr::mutate(pAD_only_smk=AD_only_smk/total_deaths)%>%
  dplyr::mutate(pAD_only_ebv=AD_only_ebv/total_deaths)%>%
  dplyr::mutate(pAD_multiple=AD_multiple/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  select( "sex", AD_prev, AD_prev_hp, AD_prev_obs, AD_prev_smk, AD_prev_ebv,
          AD_only_hp, AD_only_obs, AD_only_smk, AD_only_ebv, AD_multiple,
          AD_treat, AD_treat_no_prev,
          AD_treat_extra,
          AD_treat_prev, AD_unavoid, total_deaths,
          pAD_treat, pAD_treat_no_prev,
          pAD_treat_extra,
          pAD_prev, pAD_prev_hp, pAD_prev_obs, pAD_prev_smk, pAD_prev_ebv,
          pAD_only_hp, pAD_only_obs, pAD_only_smk, pAD_only_ebv, pAD_multiple,
          pAD_treat_prev, pAD_unavoid,
          AD_prev.asr, AD_prev_hp.asr, AD_prev_obs.asr, AD_prev_smk.asr,
          AD_prev_ebv.asr, AD_treat.asr, AD_treat_no_prev.asr,
          AD_only_hp.asr, AD_only_obs.asr, AD_only_smk.asr, AD_only_ebv.asr,
          AD_multiple.asr, AD_treat_extra.asr,
          AD_treat_prev.asr, AD_unavoid.asr, total.deaths.asr)%>%
  distinct()


table_1_1_no_chin_ind <- Avoidable_Deaths_Simulated_All %>%
  filter(!country_label%in%c("China","India"))%>%
  select(-country_label, -country_code, -cancer_code, hdi_group)%>%
  dplyr::mutate(age_cat="Overall")%>%
  dplyr::group_by(cancer, age,sex)%>%
  dplyr::mutate(py=sum(py))%>%
  ungroup()%>%
  as.data.frame()%>%
  droplevels()%>%
  ungroup()%>%
  dplyr::mutate(hdi_group=as.numeric(hdi_group))%>%
  ungroup()%>%
  dplyr::mutate(AD_treat_prev=AD_treat+AD_prev)%>%
  ungroup()%>%
  group_by(sex)%>%
  dplyr::mutate(AD_prev.asr=sum(AD_prev/py*100000*w),
                AD_prev_hp.asr=sum(AD_prev_hp/py*100000*w),
                AD_prev_obs.asr=sum(AD_prev_obs/py*100000*w),
                AD_prev_smk.asr=sum(AD_prev_smk/py*100000*w),
                AD_prev_ebv.asr=sum(AD_prev_ebv/py*100000*w),
                AD_only_hp.asr=sum(AD_only_hp/py*100000*w),
                AD_only_obs.asr=sum(AD_only_obs/py*100000*w),
                AD_only_smk.asr=sum(AD_only_smk/py*100000*w),
                AD_only_ebv.asr=sum(AD_only_ebv/py*100000*w),
                AD_multiple.asr=sum(AD_multiple/py*100000*w),
                AD_treat.asr=sum(AD_treat/py*100000*w),
                AD_treat_no_prev.asr=sum(AD_treat_no_prev/py*100000*w),
                AD_treat_extra.asr=sum(AD_treat_extra/py*100000*w),
                AD_treat_prev.asr=sum((AD_treat + AD_prev)/py*100000*w),
                AD_unavoid.asr=sum(AD_unavoid/py*100000*w),
                total.deaths.asr=sum(total_deaths2/py*100000*w)) %>%
  dplyr::mutate(total_deaths=sum(total_deaths2,na.rm=T))%>%
  dplyr::mutate(AD_treat_prev=sum(AD_treat,AD_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat=sum(AD_treat,na.rm=T))%>%
  dplyr::mutate(AD_treat_no_prev=sum(AD_treat_no_prev,na.rm=T))%>%
  dplyr::mutate(AD_treat_extra=sum(AD_treat_extra,na.rm=T))%>%
  dplyr::mutate(AD_prev=sum(AD_prev,na.rm=T))%>%
  dplyr::mutate(across(all_of(AD_FACTORS), ~ sum(.x, na.rm=T)))%>%
  dplyr::mutate(across(all_of(AD_EXCL), ~ sum(.x, na.rm=T)))%>%
  dplyr::mutate(AD_unavoid=sum(AD_unavoid,na.rm=T))%>%
  dplyr::mutate(pAD_treat=AD_treat/total_deaths)%>%
  dplyr::mutate(pAD_treat_no_prev=AD_treat_no_prev/total_deaths)%>%
  dplyr::mutate(pAD_treat_extra=AD_treat_extra/total_deaths)%>%
  dplyr::mutate(pAD_prev=AD_prev/total_deaths)%>%
  dplyr::mutate(pAD_prev_hp=AD_prev_hp/total_deaths)%>%
  dplyr::mutate(pAD_prev_obs=AD_prev_obs/total_deaths)%>%
  dplyr::mutate(pAD_prev_smk=AD_prev_smk/total_deaths)%>%
  dplyr::mutate(pAD_prev_ebv=AD_prev_ebv/total_deaths)%>%
  dplyr::mutate(pAD_only_hp=AD_only_hp/total_deaths)%>%
  dplyr::mutate(pAD_only_obs=AD_only_obs/total_deaths)%>%
  dplyr::mutate(pAD_only_smk=AD_only_smk/total_deaths)%>%
  dplyr::mutate(pAD_only_ebv=AD_only_ebv/total_deaths)%>%
  dplyr::mutate(pAD_multiple=AD_multiple/total_deaths)%>%
  dplyr::mutate(pAD_unavoid=AD_unavoid/total_deaths)%>%
  dplyr::mutate(pAD_treat_prev=AD_treat_prev/total_deaths)%>%
  select(sex, AD_prev, AD_prev_hp, AD_prev_obs, AD_prev_smk, AD_prev_ebv,
         AD_only_hp, AD_only_obs, AD_only_smk, AD_only_ebv, AD_multiple,
         AD_treat, AD_treat_no_prev,
         AD_treat_extra,
         AD_treat_prev, AD_unavoid, total_deaths,
         pAD_treat, pAD_treat_no_prev,
         pAD_treat_extra,
         pAD_prev, pAD_prev_hp, pAD_prev_obs, pAD_prev_smk, pAD_prev_ebv,
         pAD_only_hp, pAD_only_obs, pAD_only_smk, pAD_only_ebv, pAD_multiple,
         pAD_treat_prev, pAD_unavoid,
         AD_prev.asr, AD_prev_hp.asr, AD_prev_obs.asr, AD_prev_smk.asr,
         AD_prev_ebv.asr, AD_treat.asr, AD_treat_no_prev.asr,
         AD_only_hp.asr, AD_only_obs.asr, AD_only_smk.asr, AD_only_ebv.asr,
         AD_multiple.asr, AD_treat_extra.asr,
         AD_treat_prev.asr, AD_unavoid.asr, total.deaths.asr)%>%
  distinct()




# =============================================================================
# 11. Uncertainty intervals from GLOBOCAN
#     Run after section 9.
#
#     Same method as the no-risk-factor script:
#         x.low  = x - x * var.low
#         x.high = x + x * var.high
#     applied to every count, proportion and ASR, now also covering
#     AD_treat_no_prev, the four single-factor preventable estimates, the
#     exclusive pieces (AD_only_*, AD_multiple) and AD_treat_extra.
#     Column names (AD_prev.low, pAD_treat.high, AD_prev.asr.low, ...) are
#     the same as before, so anything reading the old outputs still works.
#
#     The intervals carry GLOBOCAN incidence uncertainty only -- not the
#     uncertainty in the PAFs or in survival.
# =============================================================================

# ---- measures that get an interval ------------------------------------------
UI_counts <- c("AD_prev", AD_FACTORS,
               AD_EXCL,
               "AD_treat", "AD_treat_no_prev", "AD_treat_prev",
               "AD_treat_extra",
               "AD_unavoid", "total_deaths")
UI_props  <- c("pAD_prev", paste0("p", AD_FACTORS),
               paste0("p", AD_EXCL),
               "pAD_treat", "pAD_treat_no_prev", "pAD_treat_prev",
               "pAD_treat_extra",
               "pAD_unavoid")
UI_asrs   <- c("AD_prev.asr", paste0(AD_FACTORS, ".asr"),
               paste0(AD_EXCL, ".asr"),
               "AD_treat.asr", "AD_treat_no_prev.asr", "AD_treat_prev.asr",
               "AD_treat_extra.asr",
               "AD_unavoid.asr", "total.deaths.asr")
UI_vars   <- c(UI_counts, UI_props, UI_asrs)

# each measure followed by its .low and .high
UI_cols       <- c(rbind(UI_vars,   paste0(UI_vars,   ".low"), paste0(UI_vars,   ".high")))
UI_count_cols <- c(rbind(UI_counts, paste0(UI_counts, ".low"), paste0(UI_counts, ".high")))

# labels for the formatted "estimate (low, high)" columns
UI_labels <- c(AD_prev          = "Preventable Deaths",
               AD_prev_hp       = "Preventable Deaths (H. pylori)",
               AD_prev_obs      = "Preventable Deaths (obesity)",
               AD_prev_smk      = "Preventable Deaths (smoking)",
               AD_prev_ebv      = "Preventable Deaths (EBV)",
               AD_only_hp       = "Preventable Deaths (H. pylori only)",
               AD_only_obs      = "Preventable Deaths (obesity only)",
               AD_only_smk      = "Preventable Deaths (smoking only)",
               AD_only_ebv      = "Preventable Deaths (EBV only)",
               AD_multiple      = "Preventable Deaths (interaction of two or more risk factors)",
               AD_treat         = "Treatable Deaths",
               AD_treat_no_prev = "Treatable Deaths (ignoring preventable share)",
               AD_treat_extra   = "Additional Treatable Deaths (no prevention)",
               AD_treat_prev    = "Treatable Deaths (including preventable)",
               AD_unavoid       = "Unavoidable Deaths",
               total_deaths     = "Total Deaths")

# columns kept in the formatted tables -- unavoidable left out, as before
# (remove "AD_unavoid" from the setdiff to report it)
UI_report <- setdiff(names(UI_labels), "AD_unavoid")
UI_report_cols <- unlist(lapply(UI_report, function(v) {
  lab <- UI_labels[[v]]
  if (v == "total_deaths") c(lab, paste0(lab, " (ASR)"))
  else c(lab, paste("Proportion", lab), paste0(lab, " (ASR)"))
}))


# ---- join GLOBOCAN UIs and compute .low / .high -----------------------------
add_globocan_UIs <- function(df, id_cols) {
  out <- df %>%
    ungroup() %>%
    as.data.frame() %>%
    left_join(globocan_UIs, by = c("country_code", "cancer_code", "sex"))
  n_miss <- sum(is.na(out$var.low) | is.na(out$var.high))
  if (n_miss > 0) warning(n_miss, " row(s) have no matching row in globocan_UIs; ",
                          "their intervals will be NA")
  out %>%
    mutate(across(all_of(UI_vars),
                  list(low  = ~ .x - .x * var.low,
                       high = ~ .x + .x * var.high),
                  .names = "{.col}.{.fn}")) %>%
    select(all_of(id_cols), all_of(UI_cols)) %>%
    as.data.frame()
}


# ---- round, format and paste "estimate (low, high)" -------------------------
ci_paste <- function(df, v) {
  paste0(df[[v]], " (", df[[paste0(v, ".low")]], ", ", df[[paste0(v, ".high")]], ")")
}

format_globocan_UIs <- function(df, id_cols, lt10 = FALSE) {
  fmt <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  df <- df %>%
    mutate(across(starts_with("pAD"), ~ round(., 3)*100),
           across(all_of(UI_count_cols), ~ round(., -2)),
           across(contains("asr"), ~ round(., 1)))
  if (lt10) {
    df <- df %>% mutate(across(all_of(UI_counts), ~ ifelse(. < 10, "<10", fmt(.))))
  }
  df <- df %>% mutate(across(all_of(UI_count_cols) & where(is.numeric), fmt))
  for (v in names(UI_labels)) {
    lab   <- UI_labels[[v]]
    asr_v <- if (v == "total_deaths") "total.deaths.asr" else paste0(v, ".asr")
    df[[lab]] <- ci_paste(df, v)
    if (v != "total_deaths") df[[paste("Proportion", lab)]] <- ci_paste(df, paste0("p", v))
    df[[paste0(lab, " (ASR)")]] <- ci_paste(df, asr_v)
  }
  df %>%
    select(all_of(id_cols), all_of(UI_report_cols)) %>%
    as.data.frame()
}


# =============================================================================
# 11a. By country (cancer_code 40 / "All Cancer Sites" label, as before)
# =============================================================================
AD_country_all_cancers2 <- AD_country_all_cancers %>%
  ungroup() %>%
  distinct() %>%
  add_globocan_UIs(c("country_code", "country_label", "cancer_code", "cancer", "sex")) %>%
  format_globocan_UIs(c("country_code", "country_label", "sex", "cancer_code", "cancer"),
                      lt10 = TRUE)

# numeric version to work with
AD_country_all_cancers2_editable <- AD_country_all_cancers %>%
  ungroup() %>%
  distinct() %>%
  add_globocan_UIs(c("country_code", "country_label", "cancer_code", "cancer", "sex")) %>%
  mutate(across(starts_with("pAD"), ~ round(., 3)*100)) %>%
  mutate(across(contains("asr"), ~ round(., 1)))


# =============================================================================
# 11b. By country AND cancer site, overall age-standardised (world maps)
#      Grouping: country_code, cancer_code, sex.
#      AD_treat_prev.asr = AD_prev.asr + AD_treat.asr (ASRs are additive);
#      the earlier version renamed AD_sum.asr, which is prev+treat+unavoid.
# =============================================================================
Avoidable_Deaths_Simulated_All_age_cat_overall <- Avoidable_Deaths_Simulated_All_age_cat %>%
  as.data.frame() %>%
  dplyr::filter(age_cat == "Overall") %>%
  ungroup() %>%
  dplyr::group_by(country_code, cancer_code, sex) %>%
  dplyr::mutate(AD_treat_prev.asr = AD_prev.asr + AD_treat.asr) %>%
  dplyr::mutate(total_deaths = sum(total_deaths2, na.rm = T)) %>%
  dplyr::mutate(AD_treat_prev = sum(AD_treat, AD_prev, na.rm = T)) %>%
  dplyr::mutate(AD_treat = sum(AD_treat, na.rm = T)) %>%
  dplyr::mutate(AD_treat_no_prev = sum(AD_treat_no_prev, na.rm = T)) %>%
  dplyr::mutate(AD_treat_extra = sum(AD_treat_extra, na.rm = T)) %>%
  dplyr::mutate(AD_prev = sum(AD_prev, na.rm = T)) %>%
  dplyr::mutate(across(all_of(AD_FACTORS), ~ sum(.x, na.rm = T))) %>%
  dplyr::mutate(across(all_of(AD_EXCL), ~ sum(.x, na.rm = T))) %>%
  dplyr::mutate(AD_unavoid = sum(AD_unavoid, na.rm = T)) %>%
  dplyr::mutate(pAD_treat = AD_treat/total_deaths) %>%
  dplyr::mutate(pAD_treat_no_prev = AD_treat_no_prev/total_deaths) %>%
  dplyr::mutate(pAD_treat_extra = AD_treat_extra/total_deaths) %>%
  dplyr::mutate(pAD_prev = AD_prev/total_deaths) %>%
  dplyr::mutate(pAD_prev_hp = AD_prev_hp/total_deaths) %>%
  dplyr::mutate(pAD_prev_obs = AD_prev_obs/total_deaths) %>%
  dplyr::mutate(pAD_prev_smk = AD_prev_smk/total_deaths) %>%
  dplyr::mutate(pAD_prev_ebv = AD_prev_ebv/total_deaths) %>%
  dplyr::mutate(pAD_only_hp = AD_only_hp/total_deaths) %>%
  dplyr::mutate(pAD_only_obs = AD_only_obs/total_deaths) %>%
  dplyr::mutate(pAD_only_smk = AD_only_smk/total_deaths) %>%
  dplyr::mutate(pAD_only_ebv = AD_only_ebv/total_deaths) %>%
  dplyr::mutate(pAD_multiple = AD_multiple/total_deaths) %>%
  dplyr::mutate(pAD_unavoid = AD_unavoid/total_deaths) %>%
  dplyr::mutate(pAD_treat_prev = AD_treat_prev/total_deaths) %>%
  ungroup() %>%
  distinct() %>%
  add_globocan_UIs(c("country_code", "country_label", "cancer_code", "cancer", "sex")) %>%
  format_globocan_UIs(c("country_code", "country_label", "sex", "cancer_code", "cancer"))


# =============================================================================
# 11c. By cancer site (world, country_code 900)
# =============================================================================
AD_cancer2 <- AD_cancer %>%
  add_globocan_UIs(c("country_code", "country_label", "sex", "cancer_code", "cancer")) %>%
  format_globocan_UIs(c("country_code", "country_label", "cancer_code", "cancer", "sex"))


# =============================================================================
# 11d. By region (Melanesia/Micronesia/Polynesia -> 999, as before)
# =============================================================================
AD_Region <- AD_Region2 %>%
  arrange(area) %>%
  dplyr::mutate(
    country_code = case_when(
      area %in% c(19)  ~ 999,
      !area %in% c(19) ~ country_code)) %>%
  add_globocan_UIs(c("continent", "area", "country_code", "country_label",
                     "cancer_code", "cancer", "sex")) %>%
  format_globocan_UIs(c("area", "country_code", "country_label", "sex",
                        "cancer_code", "cancer"))


# =============================================================================
# 11e. World total (Table 1)
# =============================================================================
table_1_11 <- table_1_1 %>%
  ungroup() %>%
  mutate(country_code = 900, cancer_code = 7) %>%
  add_globocan_UIs(c("country_code", "cancer_code", "sex")) %>%
  format_globocan_UIs(c("country_code", "cancer_code", "sex"))


# =============================================================================
# 11f. HDI by cancer site (country_code 981-984)
# =============================================================================
AD_by_HDI_all_cancer2 <- AD_by_HDI_all_cancer %>%
  add_globocan_UIs(c("country_code", "cancer_code", "cancer", "sex")) %>%
  format_globocan_UIs(c("country_code", "cancer_code", "cancer", "sex"))


# =============================================================================
# Outputs
# =============================================================================
AD_Region
AD_by_HDI
table_1_11
AD_by_HDI_all_cancer2
AD_country_all_cancers2
AD_cancer2
Avoidable_Deaths_Simulated_All_age_cat_overall
# =============================================================================
# 11. Plots -- scenarios by region
# =============================================================================
library(tidyverse)


theme_decomp <- theme_minimal(base_size = 15) +
  theme(legend.position    = "top",
        legend.justification = "left",
        legend.text        = element_text(size = 14, colour = PAL[["ink"]]),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
        axis.text          = element_text(colour = PAL[["ink"]]),
        strip.text.y       = element_text(angle = 0, face = "bold",
                                          colour = PAL[["ink"]], hjust = 0),
        strip.placement    = "outside",
        panel.spacing.y    = unit(10, "pt"))


PAL <- c(ink   = "#1D3040", teal  = "#2E6E6B",
         ochre = "#E4B363", rust  = "#C1502E", bone = "#EDE6DB")

SCEN <- c("AD_prev_hp"       = "Preventable (H. pylori)",
          "AD_prev"          = "Preventable (total)",
          "AD_treat"         = "Treatable",
          "AD_treat_no_prev" = "Treatable (given that no prevention takes place)",
          "AD_treat_prev"    = "Avoidable")

SCEN_COL <- c("Preventable (H. pylori)"                          = "#EB7866",
              "Preventable (total)"                              = "#4F5A8C",
              "Treatable"                                        = "#DCD747",
              "Treatable (given that no prevention takes place)" = "#6DD96F",
              "Avoidable"                                        = "#8C4F6D")

region_scen <- AD_Region2 %>%
  ungroup() %>%
  filter(sex != 0) %>%
  dplyr::group_by(country_label, sex) %>%
  dplyr::summarise(across(c(AD_prev, AD_prev_hp, AD_treat,
                            AD_treat_no_prev, AD_treat_prev, total_deaths),
                          first),
                   .groups = "drop") %>%
  dplyr::group_by(country_label) %>%
  dplyr::summarise(across(-sex, sum), .groups = "drop")

world_scen <- AD_cancer %>%
  ungroup() %>%
  filter(sex != 0) %>%
  dplyr::group_by(sex) %>%
  dplyr::summarise(across(c(AD_prev, AD_prev_hp, AD_treat,
                            AD_treat_no_prev, AD_treat_prev, total_deaths),
                          first),
                   .groups = "drop") %>%
  dplyr::summarise(across(-sex, sum)) %>%
  mutate(country_label = "World")

region_scen <- bind_rows(region_scen, world_scen)

region_order <- region_scen %>%
  arrange(AD_treat_prev) %>% pull(country_label)

counts_long <- region_scen %>%
  pivot_longer(all_of(names(SCEN)), names_to = "measure", values_to = "value") %>%
  mutate(country_label = factor(country_label, levels = region_order),
         scenario = factor(unname(SCEN[measure]), levels = unname(SCEN)))

props_long <- region_scen %>%
  mutate(across(all_of(names(SCEN)), ~ .x / total_deaths * 100)) %>%
  pivot_longer(all_of(names(SCEN)), names_to = "measure", values_to = "value") %>%
  mutate(country_label = factor(country_label, levels = region_order),
         scenario = factor(unname(SCEN[measure]), levels = unname(SCEN)))

counts_rng <- counts_long %>%
  dplyr::group_by(country_label) %>%
  dplyr::summarise(lo = min(value), hi = max(value), .groups = "drop")

props_rng <- props_long %>%
  dplyr::group_by(country_label) %>%
  dplyr::summarise(lo = min(value), hi = max(value), .groups = "drop")

p_scen_n <- ggplot(counts_long, aes(x = value, y = country_label,
                                    colour = scenario)) +
  geom_segment(data = counts_rng,
               aes(x = 0, xend = hi,
                   y = country_label, yend = country_label),
               inherit.aes = FALSE,
               colour = PAL[["ink"]], alpha = 0.30, linewidth = 0.5) +
  geom_point(size = 2.8, stroke = 0, alpha = 0.9) +
  scale_colour_manual(values = SCEN_COL, name = NULL) +
  scale_x_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0.02, 0.06))) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE,
                               override.aes = list(size = 3.5))) +
  labs(x = "Deaths avoided", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position    = "top",
        legend.text        = element_text(size = 10, colour = PAL[["ink"]]),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
        axis.text          = element_text(colour = PAL[["ink"]]))

p_scen_p <- ggplot(props_long, aes(x = value, y = country_label,
                                   colour = scenario)) +
  geom_segment(data = props_rng,
               aes(x = 0, xend = hi,
                   y = country_label, yend = country_label),
               inherit.aes = FALSE,
               colour = PAL[["ink"]], alpha = 0.30, linewidth = 0.5) +
  geom_point(size = 5, stroke = 0, alpha = 0.9) +
  geom_text(aes(label = sprintf("%.1f", value)),
            vjust = -1.1, size = 5, colour = PAL[["ink"]],
            show.legend = FALSE) +
  scale_colour_manual(values = SCEN_COL, name = NULL) +
  scale_x_continuous(limits = c(0, 80),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0.02, 0.04))) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE,
                               override.aes = list(size = 3.5))) +
  labs(x = "Proportion of avoidable deaths", y = NULL) +
  theme_minimal(base_size = 12) +
  theme(legend.position    = "top",
        legend.text        = element_text(size = 10, colour = PAL[["ink"]]),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
        axis.text          = element_text(colour = PAL[["ink"]]))

p_scen_n
p_scen_p


# =============================================================================
# 12b. Exclusive risk-factor regions on the DEATHS scale
#      Needed by sections 13 and 14. Uses the same death weight as AD_prev,
#      so the five preventable pieces sum to AD_prev exactly.
# =============================================================================
AD_overlap <- Simulated_Data_PAF_All %>%
  filter(cancer_code == 7) %>%
  mutate(
    death_w       = total_overall * ES * (1 - rel_surv),
    AD_prev       = death_w * af.comb,
    AD_only_hp    = death_w * af.hp  * (1-af.obs) * (1-af.smk) * (1-af.ebv),
    AD_only_obs   = death_w * af.obs * (1-af.hp)  * (1-af.smk) * (1-af.ebv),
    AD_only_smk   = death_w * af.smk * (1-af.hp)  * (1-af.obs) * (1-af.ebv),
    AD_only_ebv   = death_w * af.ebv * (1-af.hp)  * (1-af.obs) * (1-af.smk),
    AD_multiple   = AD_prev - (AD_only_hp + AD_only_obs + AD_only_smk + AD_only_ebv),
    AD_treat         = pmax((1-af.comb) * total_overall * (surv_ref-rel_surv) * ES, 0),
    AD_treat_no_prev = pmax(total_overall * (surv_ref-rel_surv) * ES, 0),
    AD_unavoid       = total_overall * (1 - ES*surv_ref - af.comb*ES*(1-surv_ref)),
    total_deaths2    = AD_prev + AD_treat + AD_unavoid
  )

# the five preventable pieces must sum to AD_prev
AD_overlap %>%
  summarise(diff = sum(AD_only_hp + AD_only_obs + AD_only_smk +
                         AD_only_ebv + AD_multiple, na.rm = TRUE) -
              sum(AD_prev, na.rm = TRUE))

# # =============================================================================
# 13. Preventable decomposition and treatable scenarios: region, HDI, world
# =============================================================================
MEAS <- c("AD_only_hp", "AD_only_obs", "AD_only_smk", "AD_only_ebv",
          "AD_multiple", "AD_treat", "AD_treat_no_prev", "AD_prev",
          "total_deaths2")

sum_block <- function(df, block) {
  df %>%
    dplyr::summarise(across(all_of(MEAS), ~ sum(.x, na.rm = TRUE)),
                     .groups = "drop") %>%
    mutate(block = block)
}




by_region <- AD_overlap %>%
  left_join(areas, by = "area") %>%
  filter(!is.na(country_label.y)) %>%
  dplyr::group_by(label = country_label.y) %>%
  sum_block("Region")

by_hdi <- AD_overlap %>%
  filter(hdi_group %in% 1:4) %>%
  mutate(label = factor(hdi_group, levels = 1:4,
                        labels = c("Low HDI", "Medium HDI",
                                   "High HDI", "Very high HDI"))) %>%
  dplyr::group_by(label) %>%
  sum_block("HDI") %>%
  mutate(label = as.character(label))

by_world <- AD_overlap %>%
  sum_block("World") %>%
  mutate(label = "World")

decomp <- bind_rows(by_region, by_hdi, by_world) %>%
  mutate(across(c(AD_only_hp, AD_only_obs, AD_only_smk, AD_only_ebv,
                  AD_multiple, AD_treat, AD_treat_no_prev, AD_prev),
                ~ .x / total_deaths2 * 100),
         block = factor(block, levels = c("Region", "HDI", "World")))

# row order: regions by preventable share, HDI low -> very high, world last
lab_order <- c(
  decomp %>% filter(block == "Region") %>% arrange(AD_prev) %>% pull(label),
  rev(c("Low HDI", "Medium HDI", "High HDI", "Very high HDI")),
  "World"
)
decomp <- decomp %>% mutate(label = factor(label, levels = lab_order))

# shared styling -----------------------------------------------------------
theme_decomp <- theme_minimal(base_size = 11) +
  theme(legend.position    = "top",
        legend.justification = "left",
        legend.text        = element_text(size = 9.5, colour = PAL[["ink"]]),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
        axis.text          = element_text(colour = PAL[["ink"]]),
        strip.text.y       = element_text(angle = 0, face = "bold",
                                          colour = PAL[["ink"]], hjust = 0),
        strip.placement    = "outside",
        panel.spacing.y    = unit(10, "pt"))

# ---- 13a. preventable, stacked by exclusive risk-factor region ------------
PREV <- c("AD_only_hp"  = "H. pylori only",
          "AD_only_obs" = "Obesity only",
          "AD_only_smk" = "Smoking only",
          "AD_only_ebv" = "EBV only",
          "AD_multiple" = "Interaction of two or more risk factors")

PREV_COL <- c("H. pylori only"      = "#EB7866",
              "Obesity only"        = "#4F5A8C",
              "Smoking only"        = "#DCD747",
              "EBV only"            = "#6DD96F",
              "Interaction of two or more risk factors" = "#8C4F6D")

prev_long <- decomp %>%
  dplyr::select(label, block, all_of(names(PREV))) %>%
  pivot_longer(all_of(names(PREV)), names_to = "measure", values_to = "value") %>%
  mutate(factor_lab = factor(unname(PREV[measure]), levels = rev(unname(PREV))))

p_prev_stack <- ggplot(prev_long, aes(x = value, y = label, fill = factor_lab)) +
  geom_col(width = 0.72) +
  geom_text(data = decomp,
            aes(x = AD_prev, y = label, label = sprintf("%.1f%%", AD_prev)),
            inherit.aes = FALSE, hjust = -0.2, size = 5,
            colour = PAL[["ink"]]) +
  facet_grid(block ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_fill_manual(values = PREV_COL, name = NULL,
                    breaks = unname(PREV)) +
  scale_x_continuous(limits = c(0, 85),
                     labels = function(x) paste0(x, "%"),
                     expand = c(0, 0)) +
  labs(x = "Proportion of gastric cancer deaths that are preventable",
       y = NULL) +
  theme_decomp

# ---- 13b. treatable: the two scenarios side by side -----------------------
TREAT <- c("AD_treat"         = "Treatable (main analysis, risk factor prevention takes place first",
           "AD_treat_no_prev" = "Treatable (given that no prevention takes place)")

treat_long <- decomp %>%
  dplyr::select(label, block, all_of(names(TREAT))) %>%
  pivot_longer(all_of(names(TREAT)), names_to = "measure", values_to = "value") %>%
  mutate(factor_lab = factor(unname(TREAT[measure]), levels = rev(unname(TREAT))))

p_treat_bars <- ggplot(treat_long, aes(x = value, y = label, fill = factor_lab)) +
  geom_col(position = position_dodge(width = 0.78), width = 0.72) +
  geom_text(aes(label = sprintf("%.1f", value)),
            position = position_dodge(width = 0.78),
            hjust = -0.2, size = 5, colour = PAL[["ink"]]) +
  facet_grid(block ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_fill_manual(values = c("Treatable (main analysis, risk factor prevention takes place first" = "#3FA7B5",
                               "Treatable (given that no prevention takes place)"   = "#1F6F78"),
                    name = NULL, breaks = unname(TREAT)) +
  scale_x_continuous(limits = c(0, 65),
                     labels = function(x) paste0(x, "%"),
                     expand = c(0, 0)) +
  labs(x = "Proportion of gastric cancer deaths that are treatable",
       y = NULL) +
  theme_decomp

p_prev_stack
p_treat_bars
# =============================================================================
# 13. Preventable decomposition and treatable scenarios: region, HDI, world
# =============================================================================
MEAS <- c("AD_only_hp", "AD_only_obs", "AD_only_smk", "AD_only_ebv",
          "AD_multiple", "AD_treat", "AD_treat_no_prev", "AD_prev",
          "total_deaths2")

sum_block <- function(df, block) {
  df %>%
    dplyr::summarise(across(all_of(MEAS), ~ sum(.x, na.rm = TRUE)),
                     .groups = "drop") %>%
    mutate(block = block)
}

by_region <- AD_overlap %>%
  left_join(areas, by = "area") %>%
  filter(!is.na(country_label.y)) %>%
  dplyr::group_by(label = country_label.y) %>%
  sum_block("Region")

by_hdi <- AD_overlap %>%
  filter(hdi_group %in% 1:4) %>%
  mutate(label = factor(hdi_group, levels = 1:4,
                        labels = c("Low HDI", "Medium HDI",
                                   "High HDI", "Very high HDI"))) %>%
  dplyr::group_by(label) %>%
  sum_block("HDI") %>%
  mutate(label = as.character(label))

by_world <- AD_overlap %>%
  sum_block("World") %>%
  mutate(label = "World")

decomp <- bind_rows(by_region, by_hdi, by_world) %>%
  mutate(across(c(AD_only_hp, AD_only_obs, AD_only_smk, AD_only_ebv,
                  AD_multiple, AD_treat, AD_treat_no_prev, AD_prev),
                ~ .x / total_deaths2 * 100),
         block = factor(block, levels = c("Region", "HDI", "World")))

# row order: regions by preventable share, HDI low -> very high, world last
lab_order <- c(
  decomp %>% filter(block == "Region") %>% arrange(AD_prev) %>% pull(label),
  rev(c("Low HDI", "Medium HDI", "High HDI", "Very high HDI")),
  "World"
)
decomp <- decomp %>% mutate(label = factor(label, levels = lab_order))

# shared styling -----------------------------------------------------------
theme_decomp <- theme_minimal(base_size = 11) +
  theme(legend.position    = "top",
        legend.justification = "left",
        legend.text        = element_text(size = 9.5, colour = PAL[["ink"]]),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey90", linewidth = 0.3),
        axis.text          = element_text(colour = PAL[["ink"]]),
        strip.text.y       = element_text(angle = 0, face = "bold",
                                          colour = PAL[["ink"]], hjust = 0),
        strip.placement    = "outside",
        panel.spacing.y    = unit(10, "pt"))

# ---- 13a. preventable, stacked by exclusive risk-factor region ------------
PREV <- c("AD_only_hp"  = "H. pylori only",
          "AD_only_obs" = "Obesity only",
          "AD_only_smk" = "Smoking only",
          "AD_only_ebv" = "EBV only",
          "AD_multiple" = "Interaction of two or more risk factors")

PREV_COL <- c("H. pylori only"      = "#EB7866",
              "Obesity only"        = "#4F5A8C",
              "Smoking only"        = "#DCD747",
              "EBV only"            = "#6DD96F",
              "Interaction of two or more risk factors" = "#8C4F6D")

prev_long <- decomp %>%
  dplyr::select(label, block, all_of(names(PREV))) %>%
  pivot_longer(all_of(names(PREV)), names_to = "measure", values_to = "value") %>%
  mutate(factor_lab = factor(unname(PREV[measure]), levels = rev(unname(PREV))))

p_prev_stack <- ggplot(prev_long, aes(x = value, y = label, fill = factor_lab)) +
  geom_col(width = 0.72) +
  geom_text(data = decomp,
            aes(x = AD_prev, y = label, label = sprintf("%.1f%%", AD_prev)),
            inherit.aes = FALSE, hjust = -0.2, size = 5,
            colour = PAL[["ink"]]) +
  facet_grid(block ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_fill_manual(values = PREV_COL, name = NULL,
                    breaks = unname(PREV)) +
  scale_x_continuous(limits = c(0, 85),
                     labels = function(x) paste0(x, "%"),
                     expand = c(0, 0)) +
  labs(x = "Proportion of gastric cancer deaths that are preventable",
       y = NULL) +
  theme_decomp

# ---- 13b. treatable: the two scenarios side by side -----------------------
TREAT <- c("AD_treat"         = "Treatable (main analysis, risk factor prevention takes place first)",
           "AD_treat_no_prev" = "Treatable (given that no prevention takes place)")

# colours keyed to TREAT, so the fill names and the legend labels can't disagree
TREAT_COL <- setNames(c("#3FA7B5", "#1F6F78"), unname(TREAT))

treat_long <- decomp %>%
  dplyr::select(label, block, all_of(names(TREAT))) %>%
  pivot_longer(all_of(names(TREAT)), names_to = "measure", values_to = "value") %>%
  mutate(factor_lab = factor(unname(TREAT[measure]), levels = rev(unname(TREAT))))

XMAX_T <- 65

p_treat_bars <- ggplot() +
  # shaded block bands, drawn first so they sit behind everything
  geom_rect(data = block_bounds,
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax,
                fill = fill, alpha = alpha),
            inherit.aes = FALSE) +
  scale_fill_identity() +
  scale_alpha_identity() +
  ggnewscale::new_scale_fill() +
  geom_col(data = treat_long,
           aes(x = value, y = label, fill = factor_lab),
           position = position_dodge(width = 0.78), width = 0.72) +
  geom_text(data = treat_long,
            aes(x = value, y = label, group = factor_lab,
                label = sprintf("%.1f", value)),
            position = position_dodge(width = 0.78),
            hjust = -0.14, size = 5, colour = BAND_INK) +
  # Region / HDI / World labels, in the header row of each band
  geom_text(data = block_bounds,
            aes(x = band_label(XMAX_T), y = y_header, label = block_lab),
            hjust = 0, vjust = 0.5, fontface = "bold",
            size = 5, colour = BAND_INK, alpha = 0.75) +
  scale_fill_manual(values = TREAT_COL, name = NULL, breaks = unname(TREAT)) +
  scale_x_continuous(limits = c(0, XMAX_T),
                     labels = function(x) paste0(x, "%"),
                     expand = c(0, 0)) +
  scale_y_discrete(limits = band_order, expand = expansion(add = 0.5)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(x = "Proportion of gastric cancer deaths that are treatable",
       y = NULL) +
  theme_band +
  theme(legend.position = "top",
        legend.text     = element_text(size = 17, colour = BAND_INK),
        legend.key.size = unit(20, "pt"))

p_prev_stack
p_treat_bars

# ---- 13b. treatable: with vs without prevention, arrow shows the jump -----
treat_wide <- decomp %>%
  dplyr::select(label, block, AD_treat, AD_treat_no_prev) %>%
  mutate(pct_change = (AD_treat_no_prev / AD_treat - 1) * 100,
         mid        = (AD_treat + AD_treat_no_prev) / 2)

treat_long <- treat_wide %>%
  pivot_longer(c(AD_treat, AD_treat_no_prev),
               names_to = "measure", values_to = "value") %>%
  mutate(factor_lab = factor(measure,
                             levels = c("AD_treat", "AD_treat_no_prev"),
                             labels = c("Treatable, with prevention",
                                        "Treatable")))

p_treat_arrow <- ggplot() +
  # baseline stem from zero to the with-prevention value
  geom_segment(data = treat_wide,
               aes(x = 0, xend = AD_treat, y = label, yend = label),
               colour = PAL[["ink"]], alpha = 0.20, linewidth = 0.5) +
  # arrow from with-prevention to no-prevention
  geom_segment(data = treat_wide,
               aes(x = AD_treat, xend = AD_treat_no_prev,
                   y = label, yend = label),
               colour = "#1F6F78", linewidth = 0.8,
               arrow = arrow(length = unit(5, "pt"), type = "closed")) +
  geom_point(data = treat_long,
             aes(x = value, y = label, colour = factor_lab),
             size = 3.4, stroke = 0) +
  # value labels under each dot
  geom_text(data = treat_long,
            aes(x = value, y = label, label = sprintf("%.1f", value)),
            vjust = 2, size = 5, colour = PAL[["ink"]]) +
  # % change above the arrow
  geom_text(data = treat_wide,
            aes(x = mid, y = label, label = sprintf("+%.0f%%", pct_change)),
            vjust = -0.9, size = 5, fontface = "bold", colour = "#1F6F78") +
  facet_grid(block ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_colour_manual(values = c("Treatable, with prevention" = "#3FA7B5",
                                 "Treatable, no prevention"   = "#1F6F78"),
                      name = NULL) +
  scale_x_continuous(limits = c(0, 60),
                     labels = function(x) paste0(x, "%"),
                     expand = expansion(mult = c(0, 0.02))) +
  scale_y_discrete(expand = expansion(add = 0.7)) +
  labs(x = "Proportion of gastric cancer deaths that are treatable",
       y = NULL,
      # caption = paste("Arrow: increase in the treatable share if no",
      #                 "risk-factor prevention takes place.")
) +
  theme_decomp +
  theme(plot.caption = element_text(colour = "grey45", size = 9, hjust = 0))

p_treat_arrow


# =============================================================================
# 14. Same three plots, as NUMBERS of deaths
# =============================================================================
decomp_n <- bind_rows(by_region, by_hdi, by_world) %>%
  mutate(block = factor(block, levels = c("Region", "HDI", "World")),
         label = factor(label, levels = lab_order))

# ---- 14a. preventable deaths, stacked by exclusive risk-factor region -----
prev_long_n <- decomp_n %>%
  dplyr::select(label, block, all_of(names(PREV))) %>%
  pivot_longer(all_of(names(PREV)), names_to = "measure", values_to = "value") %>%
  mutate(factor_lab = factor(unname(PREV[measure]), levels = rev(unname(PREV))))

p_prev_stack_n <- ggplot(prev_long_n, aes(x = value, y = label, fill = factor_lab)) +
  geom_col(width = 0.72) +
  geom_text(data = decomp_n,
            aes(x = AD_prev, y = label,
                label = scales::comma(round(AD_prev, -1))),
            inherit.aes = FALSE, hjust = -0.15, size = 5,
            colour = PAL[["ink"]]) +
  facet_grid(block ~ ., scales = "free", space = "free_y", switch = "y") +
  scale_fill_manual(values = PREV_COL, name = NULL, breaks = unname(PREV)) +
  scale_x_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0, 0.18))) +
  labs(x = "Number of preventable deaths among people diagnosed with gastric cancer", y = NULL) +
  theme_decomp
# ---- 14b. treatable deaths, the two scenarios side by side ----------------
# TREAT and TREAT_COL come from the 13b block, so run that first
treat_long_n <- decomp_n %>%
  dplyr::select(label, block, all_of(names(TREAT))) %>%
  pivot_longer(all_of(names(TREAT)), names_to = "measure", values_to = "value") %>%
  mutate(factor_lab = factor(unname(TREAT[measure]), levels = rev(unname(TREAT))))

# own name, so it can't overwrite XMAX_T used by the proportions plot
XMAX_TN <- max(treat_long_n$value, na.rm = TRUE) * 1.25   # room for the labels

p_treat_bars_n <- ggplot() +
  # shaded block bands, drawn first so they sit behind everything
  geom_rect(data = block_bounds,
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax,
                fill = fill, alpha = alpha),
            inherit.aes = FALSE) +
  scale_fill_identity() +
  scale_alpha_identity() +
  ggnewscale::new_scale_fill() +
  geom_col(data = treat_long_n,
           aes(x = value, y = label, fill = factor_lab),
           position = position_dodge(width = 0.78), width = 0.72) +
  geom_text(data = treat_long_n,
            aes(x = value, y = label, group = factor_lab,
                label = scales::comma(round(value, -1))),
            position = position_dodge(width = 0.78),
            hjust = -0.14, size = 5, colour = BAND_INK) +
  # Region / HDI / World labels, in the header row of each band
  geom_text(data = block_bounds,
            aes(x = band_label(XMAX_TN), y = y_header, label = block_lab),
            hjust = 0, vjust = 0.5, fontface = "bold",
            size = 5, colour = BAND_INK, alpha = 0.75) +
  scale_fill_manual(values = TREAT_COL, name = NULL, breaks = unname(TREAT)) +
  scale_x_continuous(limits = c(0, XMAX_TN), labels = scales::comma,
                     expand = c(0, 0)) +
  scale_y_discrete(limits = band_order, expand = expansion(add = 0.5)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(x = "Number of treatable deaths deaths among people diagnosed with gastric cancer", y = NULL) +
  theme_band +
  theme(legend.position = "top",
        legend.text     = element_text(size = 17, colour = BAND_INK),
        legend.key.size = unit(20, "pt"))

p_treat_bars_n
# ---- 14c. treatable deaths, arrow shows the jump --------------------------
treat_wide_n <- decomp_n %>%
  dplyr::select(label, block, AD_treat, AD_treat_no_prev) %>%
  mutate(extra = AD_treat_no_prev - AD_treat,
         mid   = (AD_treat + AD_treat_no_prev) / 2)

treat_pts_n <- treat_wide_n %>%
  pivot_longer(c(AD_treat, AD_treat_no_prev),
               names_to = "measure", values_to = "value") %>%
  mutate(factor_lab = factor(measure,
                             levels = c("AD_treat", "AD_treat_no_prev"),
                             labels = c("Treatable, with prevention",
                                        "Treatable")))

p_treat_arrow_n <- ggplot() +
  geom_segment(data = treat_wide_n,
               aes(x = 0, xend = AD_treat, y = label, yend = label),
               colour = PAL[["ink"]], alpha = 0.20, linewidth = 0.5) +
  geom_segment(data = treat_wide_n,
               aes(x = AD_treat, xend = AD_treat_no_prev,
                   y = label, yend = label),
               colour = "#1F6F78", linewidth = 0.8,
               arrow = arrow(length = unit(5, "pt"), type = "closed")) +
  geom_point(data = treat_pts_n,
             aes(x = value, y = label, colour = factor_lab),
             size = 3.4, stroke = 0) +
  geom_text(data = treat_pts_n,
            aes(x = value, y = label,
                label = scales::comma(round(value, -1))),
            vjust = 2, size = 5, colour = PAL[["ink"]]) +
  geom_text(data = treat_wide_n,
            aes(x = mid, y = label,
                label = paste0("+", scales::comma(round(extra, -1)))),
            vjust = -0.9, size = 5, fontface = "bold", colour = "#1F6F78") +
  facet_grid(block ~ ., scales = "free", space = "free_y", switch = "y") +
  scale_colour_manual(values = c("Treatable, with prevention" = "#3FA7B5",
                                 "Treatable"   = "#1F6F78"),
                      name = NULL) +
  scale_x_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0, 0.08))) +
  scale_y_discrete(expand = expansion(add = 0.7)) +
  labs(x = "Number of treatable deaths deaths among people diagnosed with gastric cancer", y = NULL,
       caption = paste("Arrow: additional treatable deaths if no",
                       "risk-factor prevention takes place.")) +
  theme_decomp +
  theme(plot.caption = element_text(colour = "grey45", size = 9, hjust = 0))

p_prev_stack_n
p_treat_bars_n
p_treat_arrow_n

# ---- preventable (by risk factor) + treatable, stacked to avoidable -------
AVOID <- c(PREV, "AD_treat" = "Treatable")

AVOID_COL <- c(PREV_COL, "Treatable" = "#2E6E6B")

# proportions -------------------------------------------------------------
avoid_long <- decomp %>%
  mutate(AD_avoidable = AD_prev + AD_treat) %>%
  dplyr::select(label, block, AD_avoidable, all_of(names(AVOID))) %>%
  pivot_longer(all_of(names(AVOID)), names_to = "measure", values_to = "value") %>%
  mutate(factor_lab = factor(unname(AVOID[measure]), levels = rev(unname(AVOID))))

avoid_tot <- decomp %>% mutate(AD_avoidable = AD_prev + AD_treat)

p_avoid_stack <- ggplot(avoid_long, aes(x = value, y = label, fill = factor_lab)) +
  geom_col(width = 0.72) +
  geom_text(data = avoid_tot,
            aes(x = AD_avoidable, y = label,
                label = sprintf("%.1f%%", AD_avoidable)),
            inherit.aes = FALSE, hjust = -0.2, size = 5, colour = PAL[["ink"]]) +
  facet_grid(block ~ ., scales = "free_y", space = "free_y", switch = "y") +
  scale_fill_manual(values = AVOID_COL, name = NULL, breaks = unname(AVOID)) +
  scale_x_continuous(limits = c(0, 90),
                     labels = function(x) paste0(x, "%"),
                     expand = c(0, 0)) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(x = "Proportion of gastric cancer deaths that are avoidable", y = NULL) +
  theme_decomp +
  theme(text        = element_text(size = 16),
        axis.text   = element_text(size = 15, colour = PAL[["ink"]]),
        axis.title  = element_text(size = 16),
        legend.text = element_text(size = 15),
        strip.text  = element_text(size = 15, face = "bold",
                                   angle = 0, hjust = 0,
                                   colour = PAL[["ink"]]))

# counts ------------------------------------------------------------------
avoid_long_n <- decomp_n %>%
  mutate(AD_avoidable = AD_prev + AD_treat) %>%
  dplyr::select(label, block, AD_avoidable, all_of(names(AVOID))) %>%
  pivot_longer(all_of(names(AVOID)), names_to = "measure", values_to = "value") %>%
  mutate(factor_lab = factor(unname(AVOID[measure]), levels = rev(unname(AVOID))))

avoid_tot_n <- decomp_n %>% mutate(AD_avoidable = AD_prev + AD_treat)

p_avoid_stack_n <- ggplot(avoid_long_n, aes(x = value, y = label, fill = factor_lab)) +
  geom_col(width = 0.72) +
  geom_text(data = avoid_tot_n,
            aes(x = AD_avoidable, y = label,
                label = scales::comma(round(AD_avoidable, -1))),
            inherit.aes = FALSE, hjust = -0.15, size = 5, colour = PAL[["ink"]]) +
  facet_grid(block ~ ., scales = "free", space = "free_y", switch = "y") +
  scale_fill_manual(values = AVOID_COL, name = NULL, breaks = unname(AVOID)) +
  scale_x_continuous(labels = scales::comma,
                     expand = expansion(mult = c(0, 0.18))) +
  guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(x = "Number of avoidable deaths deaths among people diagnosed with gastric cancer", y = NULL) +
  theme_decomp+  theme(text        = element_text(size = 16),
                       axis.text   = element_text(size = 15, colour = PAL[["ink"]]),
                       axis.title  = element_text(size = 16),
                       legend.text = element_text(size = 15),
                       strip.text  = element_text(size = 15, face = "bold",
                                                  angle = 0, hjust = 0,
                                                  colour = PAL[["ink"]]))

p_avoid_stack
p_avoid_stack_n

#combined plots

# ============================================================================
# Avoidable deaths, two panels, with block labels set INSIDE the panel on a
# tinted band rather than in facet strips. Avoids the strip-clipping problem
# in ggarrange/grid.arrange entirely.
#
# Run after decomp / decomp_n / lab_order / PREV / PREV_COL exist.
# ============================================================================

library(tidyverse)
library(ggpubr)
library(gridExtra)

# ---- block geometry -------------------------------------------------------
# On a single discrete y axis ggplot draws the FIRST factor level at the
# BOTTOM. Top to bottom we want: regions (largest first), HDI low -> very
# high, then World. So the levels run World, HDI (reversed), regions ascending.
hdi_labs   <- c("Low HDI", "Medium HDI", "High HDI", "Very high HDI")
region_asc <- setdiff(lab_order, c(hdi_labs, "World"))

# Each block gets a blank HEADER ROW at its top, inside the shaded band, so the
# block label has its own space and cannot collide with bars or totals. The
# spacers are whitespace-only levels: distinct strings that print as empty
# tick labels.
HDR_WORLD  <- " "
HDR_HDI    <- "  "
HDR_REGION <- "   "

# bottom -> top
band_order <- c("World", HDR_WORLD,
                rev(hdi_labs), HDR_HDI,
                region_asc, HDR_REGION)

decomp   <- decomp   %>% mutate(label = factor(as.character(label), levels = band_order))
decomp_n <- decomp_n %>% mutate(label = factor(as.character(label), levels = band_order))

# band = from the bottom edge of the block's first row to the top edge of its
# header row; bands are contiguous and sit behind the bars
block_bounds <- tibble(
  block     = c("World", "HDI", "Region"),
  first_row = c("World", "Very high HDI", region_asc[1]),
  header    = c(HDR_WORLD, HDR_HDI, HDR_REGION)
) %>%
  mutate(y_first  = match(first_row, band_order),
         y_header = match(header,    band_order),
         ymin     = y_first  - 0.5,
         ymax     = y_header + 0.5,
         fill  = case_when(block == "Region" ~ "#EDE6DB",
                           block == "HDI"    ~ "#2E6E6B",
                           block == "World"  ~ "#1D3040"),
         alpha = case_when(block == "Region" ~ 0.45,
                           block == "HDI"    ~ 0.10,
                           block == "World"  ~ 0.14),
         block_lab = block)

BAND_INK <- "#1D3040"

# ---- shared pieces --------------------------------------------------------
# Legend and stacking order: single risk factors by global magnitude (largest
# first), then "two or more", then treatable. Magnitude is read from the World
# row, so the order updates itself if the data change.
single_cols <- c("AD_only_hp", "AD_only_obs", "AD_only_smk", "AD_only_ebv")
rf_by_size  <- decomp %>%
  filter(label == "World") %>%
  dplyr::select(all_of(single_cols)) %>%
  unlist() %>%
  sort(decreasing = TRUE) %>%
  names()

AVOID <- c(PREV[rf_by_size],
           "AD_multiple" = PREV[["AD_multiple"]],
           "AD_treat"    = "Treatable")
AVOID_COL <- c(PREV_COL, "Treatable" = "#2E6E6B")

message("Legend order: ", paste(unname(AVOID), collapse = " > "))

make_long <- function(df) {
  df %>%
    mutate(AD_avoidable = AD_prev + AD_treat) %>%
    dplyr::select(label, block, AD_avoidable, all_of(names(AVOID))) %>%
    pivot_longer(all_of(names(AVOID)),
                 names_to = "measure", values_to = "value") %>%
    mutate(factor_lab = factor(unname(AVOID[measure]),
                               levels = rev(unname(AVOID))))
}

theme_band <- theme_minimal(base_size = 16) +
  theme(legend.position    = "none",
        axis.text          = element_text(size = 15, colour = BAND_INK),
        axis.title         = element_text(size = 16, colour = BAND_INK),
        panel.grid.minor   = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.major.x = element_line(colour = "grey88", linewidth = 0.3),
        plot.margin        = margin(12, 16, 10, 10))

# x position of the block label: just inside the left edge of the panel
band_label <- function(xmax, frac = 0.012) xmax * frac

# ---- panel A: proportions -------------------------------------------------
avoid_long <- make_long(decomp)
avoid_tot  <- decomp %>% mutate(AD_avoidable = AD_prev + AD_treat)
XMAX_P <- 100

p_avoid_stack <- ggplot() +
  geom_rect(data = block_bounds,
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax,
                fill = fill, alpha = alpha),
            inherit.aes = FALSE) +
  scale_fill_identity() +
  scale_alpha_identity() +
  ggnewscale::new_scale_fill() +
  geom_col(data = avoid_long,
           aes(x = value, y = label, fill = factor_lab), width = 0.72) +
  geom_text(data = avoid_tot,
            aes(x = AD_avoidable, y = label,
                label = sprintf("%.1f%%", AD_avoidable)),
            hjust = -0.18, size = 5, colour = BAND_INK) +
  # block label, in the header row at the top of its band
  geom_text(data = block_bounds,
            aes(x = band_label(XMAX_P), y = y_header, label = block_lab),
            hjust = 0, vjust = 0.5, fontface = "bold",
            size = 5, colour = BAND_INK, alpha = 0.75) +
  scale_fill_manual(values = AVOID_COL, name = NULL, breaks = unname(AVOID)) +
  scale_x_continuous(limits = c(0, XMAX_P),
                     labels = function(x) paste0(x, "%"),
                     expand = c(0, 0)) +
  scale_y_discrete(limits = band_order, expand = expansion(add = 0.5)) +
  labs(x = "Proportion of gastric cancer deaths that are avoidable", y = NULL) +
  theme_band

# ---- panel B: counts ------------------------------------------------------
avoid_long_n <- make_long(decomp_n)
avoid_tot_n  <- decomp_n %>% mutate(AD_avoidable = AD_prev + AD_treat)
XMAX_N <- max(avoid_tot_n$AD_avoidable, na.rm = TRUE) * 1.22

p_avoid_stack_n <- ggplot() +
  geom_rect(data = block_bounds,
            aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax,
                fill = fill, alpha = alpha),
            inherit.aes = FALSE) +
  scale_fill_identity() +
  scale_alpha_identity() +
  ggnewscale::new_scale_fill() +
  geom_col(data = avoid_long_n,
           aes(x = value, y = label, fill = factor_lab), width = 0.72) +
  geom_text(data = avoid_tot_n,
            aes(x = AD_avoidable, y = label,
                label = scales::comma(round(AD_avoidable, -2))),
            hjust = -0.14, size = 5, colour = BAND_INK) +
  geom_text(data = block_bounds,
            aes(x = band_label(XMAX_N), y = y_header, label = block_lab),
            hjust = 0, vjust = 0.5, fontface = "bold",
            size = 5, colour = BAND_INK, alpha = 0.75) +
  scale_fill_manual(values = AVOID_COL, name = NULL, breaks = unname(AVOID)) +
  scale_x_continuous(limits = c(0, XMAX_N),
                     labels = scales::comma,
                     expand = c(0, 0)) +
  scale_y_discrete(limits = band_order, expand = expansion(add = 0.5)) +
  labs(x = "Number of avoidable deaths deaths among people diagnosed with gastric cancer", y = NULL) +
  theme_band

# ---- legend, extracted once ----------------------------------------------
get_only_legend <- function(plot) {
  plot_table <- ggplot_gtable(ggplot_build(plot))
  legend_plot <- which(sapply(plot_table$grobs,
                              function(x) grepl("guide-box", x$name)))[1]
  plot_table$grobs[[legend_plot]]
}

plot1_legend <- p_avoid_stack +
  guides(fill = guide_legend(nrow = 1, byrow = TRUE, title = NULL)) +
  theme(legend.position = "top",
        legend.text     = element_text(size = 17, colour = BAND_INK),
        legend.key.size = unit(20, "pt"))

legend <- get_only_legend(plot1_legend)

# ---- stack, then attach the legend on top --------------------------------
p_A <- p_avoid_stack      # keeps its own x axis title: A is a proportion
p_B <- p_avoid_stack_n    # B is a count, so the two axes differ

combined_plot <- ggarrange(p_A, p_B,
                           nrow = 2, ncol = 1,
                           align = "v",
                           labels = c("A)", "B)"),
                           font.label = list(size = 30, color = "black"))

avoidable.AB <- grid.arrange(legend, combined_plot,
                             nrow = 2, heights = c(0.07, 0.93))

# ggsave("fig_avoidable_AB.png", avoidable.AB,
#        width = 12, height = 17, dpi = 320)


#combined treatment plot
# ---- combined treatable figure: A = proportion, B = counts -----------------
library(ggpubr)
library(gridExtra)

# defined in the bands script; repeated here only if it isn't in your session
if (!exists("get_only_legend")) {
  get_only_legend <- function(plot) {
    plot_table  <- ggplot_gtable(ggplot_build(plot))
    legend_plot <- which(sapply(plot_table$grobs,
                                function(x) grepl("guide-box", x$name)))[1]
    plot_table$grobs[[legend_plot]]
  }
}

# one shared legend, taken from panel A (both panels use the same TREAT_COL)
treat_legend <- get_only_legend(
  p_treat_bars +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE, title = NULL)) +
    theme(legend.position = "top",
          legend.text     = element_text(size = 17, colour = BAND_INK),
          legend.key.size = unit(20, "pt"))
)

# panels carry no legend; each keeps its own x axis title (proportion vs count)
p_A_treat <- p_treat_bars   + theme(legend.position = "none")
p_B_treat <- p_treat_bars_n + theme(legend.position = "none")

combined_treat <- ggarrange(p_A_treat, p_B_treat,
                            nrow = 2, ncol = 1,
                            align = "v",
                            labels = c("A)", "B)"),
                            font.label = list(size = 30, color = "black"))

treatable.AB <- grid.arrange(treat_legend, combined_treat,
                             nrow = 2, heights = c(0.10, 0.90))

# ggsave("fig_treatable_AB.png", treatable.AB,
#        width = 12, height = 17, dpi = 320, path = OUT_DIR)


#plots 

p_scen_n
p_scen_p

p_prev_stack
p_treat_bars
p_treat_arrow

p_prev_stack_n
p_treat_bars_n
p_treat_arrow_n



p_avoid_stack
p_avoid_stack_n

avoidable.AB
treatable.AB

ggsave("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024\\Stacked_bars_prop_fig_1.png",
       plot = p_avoid_stack, width = 20, height = 15, dpi = 500)

ggsave("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024\\Stacked_bars_n__fig_1.png",
       plot = p_avoid_stack_n, width = 20, height = 15, dpi = 500)


ggsave("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024\\Stacked_treat_n__fig_1.png",
       plot = p_treat_bars_n, width = 20, height = 15, dpi = 500)

ggsave("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024\\Stacked_bars_n_prop_fig_1_final.png",
       plot = avoidable.AB, width = 15, height = 20, dpi = 500)

ggsave("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024\\Stacked_treat_fig_3_final.png",
       plot = treatable.AB, width = 15, height = 22, dpi = 500)


treatable.AB


#pdf outputs


ggsave("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024\\Stacked_bars_prop_fig_1.pdf",
       plot = p_avoid_stack,  width = 20, height = 15, dpi = 500)


ggsave("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024\\Stacked_bars_n__fig_1.pdf",
       plot = p_avoid_stack_n,  width = 20, height = 15, dpi = 500)


ggsave("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024\\Stacked_treat_n__fig_1.pdf",
       plot = p_treat_bars_n,  width = 20, height = 15, dpi = 500)


ggsave("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024\\Stacked_bars_n_prop_fig_1_final.pdf",
       plot = avoidable.AB, width = 15, height = 20, dpi = 500)

ggsave("\\\\Inti\\cin\\Studies\\Survival\\SurvCan\\Data\\_work\\Oliver_Langselius\\AD_PREV_TREAT\\Gastric2024\\Stacked_treat_fig_3_final.pdf",
       plot = treatable.AB, width = 15, height = 22, dpi = 500)


#cehcks for the paper

rank_check <- decomp %>%
  filter(block == "Region") %>%
  mutate(AD_avoidable = AD_prev + AD_treat) %>%
  dplyr::select(label, AD_avoidable, AD_only_hp, AD_only_obs,
                AD_only_smk, AD_only_ebv, AD_multiple) %>%
  mutate(hp_share_of_avoidable = AD_only_hp / AD_avoidable * 100)

# claim 1: H. pylori > half of avoidable deaths in every region?
rank_check %>%
  summarise(n_regions = n(),
            n_over_50 = sum(hp_share_of_avoidable > 50),
            lowest    = min(hp_share_of_avoidable),
            lowest_in = label[which.min(hp_share_of_avoidable)])

# claim 2: which factor ranks second, excluding "two or more"?
second <- rank_check %>%
  dplyr::select(label, AD_only_hp, AD_only_obs, AD_only_smk, AD_only_ebv) %>%
  pivot_longer(-label, names_to = "factor", values_to = "value") %>%
  group_by(label) %>%
  arrange(desc(value), .by_group = TRUE) %>%
  mutate(rank = row_number()) %>%
  ungroup()

second %>%
  filter(rank == 1) %>%
  count(factor, name = "n_regions_first")

second %>%
  filter(rank == 2) %>%
  count(factor, name = "n_regions_second")

second %>%
  filter(rank == 3) %>%
  count(factor, name = "n_regions_third")

# the full picture, one row per region
second %>%
  mutate(factor = str_remove(factor, "AD_only_")) %>%
  group_by(label) %>%
  summarise(order = paste(factor, collapse = " > "), .groups = "drop") %>%
  print(n = Inf)
