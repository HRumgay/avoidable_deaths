# ============================================================================
# Plots for the REGISTRY-level Nordpred output.
# Run AFTER the main script -- uses asr, future_py, reg_period, fit_summary,
# pred_counts_total, counts_2050.
#
# Everything here refers to the registry catchment population. Observed and
# projected counts are therefore on the same footing -- there is no step at
# the join from national scaling.
# ============================================================================

library(tidyverse)

# OUT_DIR <- "plots"
# dir.create(OUT_DIR, showWarnings = FALSE)

sex_label <- function(s) if_else(s == 1L, "Male", "Female")
SEX_COL   <- c(Male = "#1f78b4", Female = "#e31a1c")

# which registry/sex/period combinations are PROJECTED rather than observed
pred_period_lookup <- bind_rows(future_py) %>%
  distinct(key, period) %>%
  mutate(is_pred = TRUE)

# order facets by 2050 burden so the big contributors come first
registry_order <- counts_2050 %>%
  group_by(registry_label) %>%
  summarise(total = sum(cases_pred), .groups = "drop") %>%
  arrange(desc(total)) %>%
  pull(registry_label)

# ---------------------------------------------------------------------------
# 1. Age-standardised rate trajectories, observed solid / projected dashed
# ---------------------------------------------------------------------------
asr_plot_data <- asr %>%
  left_join(pred_period_lookup, by = c("key", "period")) %>%
  mutate(is_pred        = replace_na(is_pred, FALSE),
         sex_label      = sex_label(sex),
         registry_label = factor(registry_label, levels = registry_order))

# the projected line starts at the last observed point so the series connects
obs_seg  <- asr_plot_data %>% filter(!is_pred)
pred_seg <- asr_plot_data %>%
  group_by(key) %>%
  filter(is_pred | period == max(period[!is_pred])) %>%
  ungroup()

p_asr <- ggplot(mapping = aes(period, asr_world, colour = sex_label)) +
  geom_line(data = obs_seg,  linewidth = 0.6) +
  geom_line(data = pred_seg, linewidth = 0.6, linetype = "22") +
  facet_wrap(~ registry_label, scales = "free_y") +
  scale_colour_manual(values = SEX_COL) +
  labs(title    = "Age-standardised incidence, observed and projected",
       subtitle = "Solid = observed periods; dashed = Nordpred projection",
       x = NULL, y = "ASR (World), per 100,000 py", colour = NULL) +
  theme_minimal(base_size = 9) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", size = 7))

p_asr

# ggsave(file.path(OUT_DIR, "asr_by_registry.png"), p_asr,
#        width = 13, height = 9, dpi = 200)

# ---------------------------------------------------------------------------
# 2. Case counts over time: observed vs projected, registry population
# ---------------------------------------------------------------------------
# observed: only the fitted window
obs_counts <- reg_period %>%
  group_by(registry, sex, period) %>%
  summarise(cases = sum(cases), .groups = "drop") %>%
  inner_join(fit_summary %>%
               dplyr::select(key, registry, sex, registry_label, first_period),
             by = c("registry", "sex")) %>%
  filter(period >= first_period) %>%
  dplyr::select(key, registry_label, sex, period, cases) %>%
  mutate(source = "Observed")

proj_counts <- pred_counts_total %>%
  dplyr::select(key, registry_label, sex, period, cases = cases_pred) %>%
  mutate(source = "Projected")

# start the projected line at the last observed point so it connects
proj_counts <- bind_rows(
  obs_counts %>%
    group_by(key) %>%
    slice_max(period, n = 1, with_ties = FALSE) %>%
    ungroup() %>%
    mutate(source = "Projected"),
  proj_counts
)

counts_plot_data <- bind_rows(obs_counts, proj_counts) %>%
  mutate(sex_label      = sex_label(sex),
         registry_label = factor(registry_label, levels = registry_order))

p_counts <- ggplot(counts_plot_data,
                   aes(period, cases, colour = sex_label, linetype = source,
                       group = interaction(key, source))) +
  geom_line(linewidth = 0.6) +
  facet_wrap(~ registry_label, scales = "free_y") +
  scale_colour_manual(values = SEX_COL) +
  scale_linetype_manual(values = c("Observed" = "solid", "Projected" = "22")) +
  scale_y_continuous(labels = scales::comma) +
  labs(title    = "Cases per 5-year period",
       subtitle = "Registry catchment population, observed and projected",
       x = NULL, y = "Cases", colour = NULL, linetype = NULL) +
  theme_minimal(base_size = 9) +
  theme(legend.position = "top",
        panel.grid.minor = element_blank(),
        strip.text = element_text(face = "bold", size = 7))

p_counts

# ggsave(file.path(OUT_DIR, "counts_by_registry.png"), p_counts,
#        width = 13, height = 9, dpi = 200)

# ---------------------------------------------------------------------------
# 3. Single-year (SINGLE_YEAR) counts by registry
# ---------------------------------------------------------------------------
single_year_plot_data <- counts_2050 %>%
  mutate(sex_label = sex_label(sex)) %>%
  group_by(registry_label) %>%
  mutate(total = sum(cases_pred)) %>%
  ungroup() %>%
  mutate(registry_label = fct_reorder(registry_label, total))

p_single <- ggplot(single_year_plot_data,
                   aes(cases_pred, registry_label, fill = sex_label)) +
  geom_col() +
  scale_fill_manual(values = SEX_COL) +
  scale_x_continuous(labels = scales::comma) +
  labs(title    = paste0("Projected cases in ", SINGLE_YEAR),
       subtitle = paste0("Registry catchment population; rate of the ",
                         period_label(to_period(SINGLE_YEAR)), " period"),
       x = "Cases", y = NULL, fill = NULL) +
  theme_minimal(base_size = 10) +
  theme(legend.position = "top", panel.grid.minor = element_blank())

p_single

# ggsave(file.path(OUT_DIR, "cases_2050_by_registry.png"), p_single,
#        width = 8, height = 9, dpi = 200)