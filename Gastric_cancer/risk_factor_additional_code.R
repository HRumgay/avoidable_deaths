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
            hjust = -0.18, size = 20/.pt, colour = BAND_INK) +
  # block label, in the header row at the top of its band
  geom_text(data = block_bounds,
            aes(x = band_label(XMAX_P), y = y_header, label = block_lab),
            hjust = 0, vjust = 0.5, fontface = "bold",
            size = 19/.pt, colour = BAND_INK, alpha = 0.75) +
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
            hjust = -0.14, size = 20/.pt, colour = BAND_INK) +
  geom_text(data = block_bounds,
            aes(x = band_label(XMAX_N), y = y_header, label = block_lab),
            hjust = 0, vjust = 0.5, fontface = "bold",
            size = 19/.pt, colour = BAND_INK, alpha = 0.75) +
  scale_fill_manual(values = AVOID_COL, name = NULL, breaks = unname(AVOID)) +
  scale_x_continuous(limits = c(0, XMAX_N),
                     labels = scales::comma,
                     expand = c(0, 0)) +
  scale_y_discrete(limits = band_order, expand = expansion(add = 0.5)) +
  labs(x = "Avoidable gastric cancer deaths", y = NULL) +
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