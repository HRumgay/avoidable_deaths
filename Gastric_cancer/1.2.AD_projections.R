# ============================================================================
# Gastric cancer: change 2024 -> 2050 in incidence ASR, cases, avoidable-death
# ASR and avoidable deaths, pooled across CI5plus registries (Nordpred APC).
#
# Needs in the session:
#   CI5_plus, UN_Pop_Projections,
#   AD_country_all_cancers  (from the avoidable-deaths pipeline; gives pAD)
#
# Method in one paragraph: Nordpred is fitted per registry x sex on the 5 most
# recent 5-year periods. Registry person-years for projected periods = UN
# national projection x the registry's age-specific share of its country in
# the last observed period. Cases for a year = rate of the period containing
# that year x registry person-years in that year. Avoidable deaths = cases x
# the COUNTRY's proportion of deaths avoidable (pAD_prev + pAD_treat).
# Nothing is scaled up to national totals.
# ============================================================================

library(tidyverse)
source("C://Users//langseliuso//Downloads//Nordpred.S")

# ---- settings ---------------------------------------------------------------
CANCER_CODE      <- 4
PERIOD_LEN       <- 5
N_PERIODS        <- 5          # fit on the 5 most recent periods (25 years)
CUTTREND_BASE    <- c(0, .25, .5, .75, .75)
LAST_PRED_PERIOD <- 2050       # last projected period = 2050-54
YEARS            <- c(2024, 2050)
MIN_CASES_PER_AGE <- 10
MIN_MODELLED_AGES <- 6
N_AGE            <- 18
UN_SCALE         <- 1000       # WPP is in thousands

AGE_LABELS <- c(paste0(seq(0, 80, 5), "-", seq(4, 84, 5)), "85+")
WSTAND     <- c(0.12, 0.10, 0.09, 0.09, 0.08, 0.08, 0.06, 0.06, 0.06,
                0.06, 0.05, 0.04, 0.04, 0.03, 0.02, 0.01, 0.005, 0.005)

to_period    <- function(y) (y %/% PERIOD_LEN) * PERIOD_LEN
period_label <- function(p) sprintf("%d-%02d", p, (p + PERIOD_LEN - 1) %% 100)
sex_lab      <- function(s) if_else(s == 1L, "Male", "Female")

make_cuttrend <- function(n) {
  if (n <= length(CUTTREND_BASE)) CUTTREND_BASE[seq_len(n)]
  else c(CUTTREND_BASE, rep(tail(CUTTREND_BASE, 1), n - length(CUTTREND_BASE)))
}

# Nordpred refuses more than 5 projection periods; data ending 2010-14 need 8
# to reach 2050-54, so the guard is removed. Later periods lean on a borrowed
# cohort effect -- say so in the methods.
stmts <- as.list(body(nordpred.estimate))
drop  <- vapply(stmts, function(e)
  grepl("can not predict more than 5 periods",
        paste(deparse(e), collapse = " "), fixed = TRUE), logical(1))
if (any(drop)) {
  body(nordpred.estimate) <- as.call(c(as.name("{"), stmts[-1][!drop[-1]]))
} else {
  warning("Could not find the 5-period guard: horizon capped at 5 periods.")
}

# ============================================================================
# 1. Registry data -> registry x sex x age x 5-year period
# ============================================================================
CI5_use <- as_tibble(CI5_plus) %>%
  filter(age >= 1, age <= N_AGE) %>%                  # age 19 = unknown
  mutate(cancer_num = suppressWarnings(as.numeric(as.character(cancer))))
stopifnot(CANCER_CODE %in% CI5_use$cancer_num)

# keep the all-population series where a registry also reports ethnic subgroups
if ("ethnic_group" %in% names(CI5_use)) {
  eth_mixed <- CI5_use %>% distinct(registry, ethnic_group) %>%
    count(registry) %>% filter(n > 1)
  CI5_use <- CI5_use %>%
    filter(!registry %in% eth_mixed$registry | ethnic_group == 99)
}

# py is repeated on every cancer row: take distinct values, never sum
py_reg <- CI5_use %>%
  distinct(registry, registry_label, sex, year, age, py) %>%
  group_by(registry, registry_label, sex, year, age) %>%
  summarise(py = max(py), .groups = "drop")

cases_reg <- CI5_use %>%
  filter(cancer_num == CANCER_CODE) %>%
  group_by(registry, sex, year, age) %>%
  summarise(cases = sum(cases, na.rm = TRUE), .groups = "drop")

reg_period <- py_reg %>%
  left_join(cases_reg, by = c("registry", "sex", "year", "age")) %>%
  mutate(cases = replace_na(cases, 0), period = to_period(year)) %>%
  group_by(registry, registry_label, sex, age, period) %>%
  summarise(cases = sum(cases), py = sum(py),
            n_years = n_distinct(year), .groups = "drop") %>%
  filter(n_years == PERIOD_LEN) %>%                   # complete periods only
  dplyr::select(-n_years) %>%
  group_by(registry, sex, period) %>%
  filter(n_distinct(age) == N_AGE) %>%                # all 18 age groups
  ungroup()

# ============================================================================
# 2. Registry -> UN country code (only to find its population projection)
# ============================================================================
un_countries <- UN_Pop_Projections %>% distinct(country_label, country_code)

prefix_rules <- tribble(
  ~pattern,            ~un_label,
  "^USA",              "United States of America",
  "^UK",               "United Kingdom",
  "^Canada",           "Canada",
  "^China, Hong Kong", "China, Hong Kong SAR",
  "^China, Macao",     "China, Macao SAR",
  "^Taiwan",           "China, Taiwan Province of China"
)
name_alias <- c(
  "Czech Republic"  = "Czechia",
  "The Netherlands" = "Netherlands",
  "Korea"           = "Republic of Korea",
  "Russia"          = "Russian Federation",
  "Iran"            = "Iran (Islamic Republic of)",
  "Turkey"          = "Turkiye",
  "Vietnam"         = "Viet Nam",
  "Bolivia"         = "Bolivia (Plurinational State of)",
  "Tanzania"        = "United Republic of Tanzania",
  "Moldova"         = "Republic of Moldova",
  "Brunei"          = "Brunei Darussalam"
)
match_country <- function(label) {
  hit <- prefix_rules$un_label[str_detect(label, prefix_rules$pattern)]
  if (length(hit)) return(hit[1])
  key <- label %>% str_remove(":.*$") %>% str_extract("^[^,]+") %>% str_trim()
  key <- unname(coalesce(name_alias[key], key))
  if (key %in% un_countries$country_label) return(key)
  cand <- un_countries$country_label[startsWith(un_countries$country_label, key)]
  if (length(cand) == 1) cand else NA_character_
}

crosswalk <- reg_period %>%
  distinct(registry, registry_label) %>%
  mutate(un_label = map_chr(registry_label, match_country)) %>%
  left_join(un_countries, by = c("un_label" = "country_label"))

if (any(is.na(crosswalk$country_code))) {
  message("Registries with no UN match (dropped):")
  print(crosswalk %>% filter(is.na(country_code)) %>%
          dplyr::select(registry, registry_label), n = Inf)
}
reg_period <- reg_period %>%
  inner_join(crosswalk %>% filter(!is.na(country_code)) %>%
               dplyr::select(registry, country_code), by = "registry")

# ============================================================================
# 3. UN national populations, in persons, on the 18 CI5 age groups
# ============================================================================
un_ages_long <- function(df) {
  df %>%
    transmute(
      sex, country_code, year,
      `1`  = age1,  `2`  = age2,  `3`  = age3,  `4`  = age4,
      `5`  = age5,  `6`  = age6,  `7`  = age7,  `8`  = age8,
      `9`  = age9,  `10` = age10, `11` = age11, `12` = age12,
      `13` = age13, `14` = age14, `15` = age15, `16` = age16,
      `17` = age17,
      `18` = age18 + age19 + age20 + age100p
    ) %>%
    pivot_longer(`1`:`18`, names_to = "age", values_to = "pop") %>%
    mutate(age = as.integer(age), pop = pop * UN_SCALE)
}

# person-years per 5-year period (sum of the five annual populations)
un_period <- UN_Pop_Projections %>%
  filter(sex %in% c(1, 2)) %>%
  un_ages_long() %>%
  mutate(period = to_period(year)) %>%
  group_by(country_code, sex, age, period) %>%
  summarise(py_un = sum(pop), n_years = n(), .groups = "drop") %>%
  filter(n_years == PERIOD_LEN) %>%
  dplyr::select(-n_years)

# population in the single reporting years (one year = one year of person-years)
un_years <- UN_Pop_Projections %>%
  filter(sex %in% c(1, 2), year %in% YEARS) %>%
  un_ages_long() %>%
  dplyr::select(country_code, sex, age, year, py_un_year = pop)

# ============================================================================
# 4. Fit Nordpred, one registry x sex at a time
# ============================================================================
mat_from <- function(df, value_col, periods) {
  m <- df %>%
    dplyr::select(age, period, val = all_of(value_col)) %>%
    filter(period %in% periods) %>%
    pivot_wider(names_from = period, values_from = val) %>%
    arrange(age)
  if (!identical(m$age, 1:N_AGE)) return(NULL)
  out <- as.matrix(m[, as.character(periods), drop = FALSE])
  dimnames(out) <- list(AGE_LABELS, period_label(periods))
  out
}

# youngest age from which every older group has >= MIN_CASES_PER_AGE cases
# (85+ may be sparse)
find_startage <- function(cnt) {
  for (a in seq_len(N_AGE - MIN_MODELLED_AGES + 1))
    if (all(cnt[a:N_AGE] >= MIN_CASES_PER_AGE)) return(a)
  for (a in seq_len(N_AGE - MIN_MODELLED_AGES + 1))
    if (all(cnt[a:(N_AGE - 2)] >= MIN_CASES_PER_AGE)) return(a)
  NA_integer_
}

combos <- reg_period %>%
  distinct(registry, registry_label, country_code, sex) %>%
  arrange(registry, sex)

fits <- list(); fit_info <- list(); cov_list <- list()
skipped <- character()

for (i in seq_len(nrow(combos))) {
  
  reg <- combos$registry[i]; lab <- combos$registry_label[i]
  cc  <- combos$country_code[i]; sx <- combos$sex[i]
  key <- paste0("reg", reg, "_sex", sx)
  
  obs <- reg_period %>% filter(registry == reg, sex == sx)
  ps  <- sort(unique(obs$period))
  
  if (length(ps) < N_PERIODS) { skipped[key] <- "under 25 years"; next }
  ps <- tail(ps, N_PERIODS)
  if (any(diff(ps) != PERIOD_LEN)) { skipped[key] <- "gap in periods"; next }
  last_p <- max(ps)
  
  n_pred <- (LAST_PRED_PERIOD - last_p) / PERIOD_LEN
  if (n_pred < 1) { skipped[key] <- "no period left to project"; next }
  fut_p <- last_p + PERIOD_LEN * seq_len(n_pred)
  
  cases_mat <- mat_from(obs, "cases", ps)
  py_obs    <- mat_from(obs, "py",    ps)
  if (is.null(cases_mat) || is.null(py_obs) || anyNA(cases_mat) ||
      anyNA(py_obs) || any(py_obs <= 0)) {
    skipped[key] <- "incomplete data"; next
  }
  # a zero-case matrix does not make Nordpred fail: it returns ~1e-14 rates
  if (sum(cases_mat) < 25) { skipped[key] <- "under 25 cases"; next }
  
  est_age <- find_startage(rowSums(cases_mat))
  if (is.na(est_age)) { skipped[key] <- "too sparse by age"; next }
  
  un      <- un_period %>% filter(country_code == cc, sex == sx)
  un_last <- mat_from(un, "py_un", last_p)
  un_fut  <- mat_from(un, "py_un", fut_p)
  if (is.null(un_last) || is.null(un_fut) || anyNA(un_last) || anyNA(un_fut)) {
    skipped[key] <- "no UN population"; next
  }
  
  # registry share of its country, per age group, in the last observed period
  cov <- as.vector(py_obs[, ncol(py_obs)]) / as.vector(un_last[, 1])
  if (anyNA(cov) || any(cov <= 0)) { skipped[key] <- "bad coverage ratio"; next }
  
  res <- tryCatch(
    nordpred(cases       = as.data.frame(cases_mat, check.names = FALSE),
             pyr         = as.data.frame(cbind(py_obs, un_fut * cov),
                                         check.names = FALSE),
             startestage = est_age,
             startuseage = min(est_age + 1L, N_AGE),
             noperiods   = N_PERIODS,
             cuttrend    = make_cuttrend(n_pred)),
    error = function(e) NULL)
  if (is.null(res) || anyNA(as.matrix(res$predictions))) {
    skipped[key] <- "Nordpred failed or returned NA"; next
  }
  
  fits[[key]]     <- res
  fit_info[[key]] <- tibble(key = key, registry = reg, registry_label = lab,
                            country_code = cc, sex = sx)
  cov_list[[key]] <- tibble(key = key, age = 1:N_AGE, cov = cov)
}

if (length(fits) == 0) stop("No registry/sex series could be fitted.")
fit_summary <- bind_rows(fit_info)
cov_by_key  <- bind_rows(cov_list)

cat("\nSeries excluded, by reason:\n"); print(table(skipped))

# ============================================================================
# 5. Cases and avoidable deaths in 2024 and 2050, per registry x sex x age
# ============================================================================
# Nordpred age-specific rates, observed + projected periods; keep the two
# periods that contain the reporting years (2020-24 and 2050-54)
pred_rates <- map_dfr(names(fits), function(k) {
  nordpred.getpred(fits[[k]], incidence = TRUE) %>%
    as.data.frame() %>%
    rownames_to_column("age_group") %>%
    pivot_longer(-age_group, names_to = "period_label",
                 values_to = "rate_per_100k") %>%
    mutate(key = k)
}) %>%
  mutate(age    = match(age_group, AGE_LABELS),
         period = as.integer(str_sub(period_label, 1, 4))) %>%
  filter(period %in% to_period(YEARS))

# the country's proportion of deaths avoidable (prevention + treatment).
# No cancer_code filter: this object is gastric-only, coded 7 or 40.
pAD_country <- AD_country_all_cancers %>%
  filter(sex %in% c(1, 2)) %>%
  distinct(country_code, sex, pAD_prev, pAD_treat) %>%
  mutate(pAD_av = pAD_prev + pAD_treat)
stopifnot(!anyDuplicated(pAD_country[c("country_code", "sex")]))

year_age <- pred_rates %>%
  mutate(year = YEARS[match(period, to_period(YEARS))]) %>%
  left_join(fit_summary, by = "key") %>%
  left_join(cov_by_key,  by = c("key", "age")) %>%
  left_join(un_years,    by = c("country_code", "sex", "age", "year")) %>%
  left_join(pAD_country, by = c("country_code", "sex")) %>%
  filter(!is.na(pAD_av)) %>%               # same series feed every number below
  mutate(py    = py_un_year * cov,         # registry person-years in that year
         cases = rate_per_100k / 1e5 * py,
         AD    = cases * pAD_av)

# ============================================================================
# 6. Output
# ============================================================================
# pool across registries at the age-specific level, then age-standardise
pool <- function(df, ...) {
  df %>%
    group_by(year, ..., age) %>%
    summarise(cases = sum(cases), AD = sum(AD), py = sum(py), .groups = "drop") %>%
    mutate(w = WSTAND[age]) %>%
    group_by(year, ...) %>%
    summarise(n_cases = sum(cases), n_AD = sum(AD),
              asr_inc = sum(cases / py * 1e5 * w),
              asr_AD  = sum(AD    / py * 1e5 * w),
              .groups = "drop")
}
add_pct <- function(pooled) {
  pooled %>%
    pivot_wider(names_from = year,
                values_from = c(n_cases, n_AD, asr_inc, asr_AD)) %>%
    mutate(pct_asr_incidence = (asr_inc_2050 / asr_inc_2024 - 1) * 100,
           pct_asr_AD        = (asr_AD_2050  / asr_AD_2024  - 1) * 100,
           pct_n_cases       = (n_cases_2050 / n_cases_2024 - 1) * 100,
           pct_n_AD          = (n_AD_2050    / n_AD_2024    - 1) * 100)
}
show <- function(df) print(as.data.frame(df %>% mutate(across(where(is.numeric),
                                                              ~ round(.x, 1)))))

cat("\n== Included ==\n")
show(tibble(registries_fitted = n_distinct(fit_summary$registry),
            countries_fitted  = n_distinct(fit_summary$country_code),
            series_fitted     = nrow(fit_summary),
            registries_in_pooled_results = n_distinct(year_age$registry),
            countries_in_pooled_results  = n_distinct(year_age$country_code)))

# (1) all registries, both sexes: the headline percentages
change_all <- year_age %>% pool() %>% add_pct()
cat("\n== Pooled change 2024 -> 2050, both sexes ==\n")
show(change_all %>% dplyr::select(n_AD_2024, n_AD_2050, starts_with("pct_")))

# (2) by sex
change_sex <- year_age %>% mutate(sex_label = sex_lab(sex)) %>%
  pool(sex_label) %>% add_pct()
cat("\n== Pooled change by sex ==\n")
show(change_sex %>% dplyr::select(sex_label, starts_with("pct_")))

# (3) direction by registry and sex: how many fall / rise, and the mean change
#     within each group (unweighted across registries)
change_registry <- year_age %>% mutate(sex_label = sex_lab(sex)) %>%
  pool(key, country_code, sex_label) %>% add_pct()

direction <- change_registry %>%
  mutate(direction = if_else(pct_asr_incidence < 0, "ASR down", "ASR up")) %>%
  group_by(sex_label, direction) %>%
  summarise(n_registries    = n(),
            n_countries     = n_distinct(country_code),
            mean_pct_asr    = mean(pct_asr_incidence),
            mean_pct_cases  = mean(pct_n_cases),
            mean_pct_AD     = mean(pct_n_AD),
            .groups = "drop")
cat("\n== Direction of the incidence ASR, 2024 -> 2050 ==\n")
show(direction)

# (4) the illustrative global figure: the pooled % change in avoidable deaths
#     applied to the 2024 global estimate. Fill in your own numbers.
GLOBAL_AD_2024 <- c(estimate = 429500, lower = 424000, upper = 435100)
GLOBAL_AD_2050 <- GLOBAL_AD_2024 * (1 + change_all$pct_n_AD / 100)
cat("\n== Illustrative global avoidable deaths ==\n")
print(round(rbind(`2024` = GLOBAL_AD_2024, `2050` = GLOBAL_AD_2050), -2))


#seeing increase and decrease
year_age %>%
  pool(registry, country_code) %>%
  add_pct() %>%
  summarise(registries = n(),
            asr_down   = sum(pct_asr_incidence < 0),
            asr_up     = sum(pct_asr_incidence > 0))
