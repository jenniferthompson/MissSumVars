################################################################################
## Create visuals for JSM 2018 presentation
################################################################################

library(visdat)
library(naniar)
library(ggplot2)
library(extrafont)
# library(gganimate)
library(tidyverse)
library(viridisLite)
library(patchwork)

## viridisLite::viridis(n = 4, direction = -1)
## FDE725 = yellow
## 35B779 = green
## 31688E = blue
## 440154 = purple

vcolors <- viridisLite::viridis(n = 4, direction = -1) %>%
  set_names(c("yellow", "green", "blue", "purple"))

## -- Function for combining/spacing plots from Andrew Heiss -------------------
make_plot <- function(cut, omit_y = FALSE) {
  p <- ggplot(data = filter(diamonds, cut == cut, color %in% LETTERS[4:5]),
              aes(x = carat, y = price)) +
    geom_point() +
    facet_wrap(~ color) +
    labs(title = cut)
  
  if (omit_y) {
    p + theme(axis.title.y = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank())
  } else {
    p
  }
  
}

# make_plot("Fair") + make_plot("Good", omit_y = TRUE)

## -- Compare prospective, retrospective studies -------------------------------
prosp_data <- readRDS("../rawdata/study1.rds")
retro_data <- readRDS("../rawdata/study2.rds")

pres_vis_miss <- function(df){
  vis_miss(subset(df, select = c(status))) +
    scale_fill_manual(values = as.character(vcolors[c("purple", "yellow")])) +
    theme_minimal(base_family = "Roboto Condensed") +
    theme(
      legend.position = "none",
      axis.title = element_blank(),
      axis.text.x = element_text(size = 20, face = "bold", color = "#FFFFFF"),
      axis.title.y = element_blank(),
      axis.text.y = element_blank(),
      panel.grid = element_blank(),
      ## Couldn't figure out transparent background. Hacked it by matching
      ## default theme background.
      plot.background = element_rect(fill = "#272822", colour = NA)
      ## Switched to default.
      # plot.background = element_rect(fill = "#272822", colour = "#7a7a7a")
    )
}

vm_prosp <- pres_vis_miss(prosp_data)
vm_retro <- pres_vis_miss(retro_data)

ggsave(
  filename = "prosp_vismiss.png",
  pres_vis_miss(prosp_data),
  path = "presfigures",
  device = "png",
  # type = "cairo",
  width = 5, height = 5.25, units = "in", dpi = 400
)

ggsave(
  filename = "retro_vismiss.png",
  vm_retro,
  path = "presfigures",
  device = "png",
  # type = "cairo",
    ## weirdly, retrospective missingness didn't show up with Cairo, even though
    ## prospective did <shrug>
  width = 5, height = 5.25, units = "in", dpi = 400
)

## -- Simulation Results -------------------------------------------------------
## Load and combine simulation results, all stored in results/ in chunks of 250
sim_files <- list.files("../results", pattern = "^sim\\_results\\_.*\\.rds$")
sim_results <- map(paste0("../results/", sim_files), readRDS) %>% bind_rows() 

## Bias: Mean of betas - true beta
## SE: Mean estimated SEs
## CI width: mean width of the (95%) confidence interval
## Power: % of reps where estimate CI does not include 0
## Coverage: % of reps where estimate CI includes true beta

sim_results2 <- sim_results %>%
  mutate(
    ## "delete" and "impute" are confusing since they both involve imputation;
    ## change to "impute summary" and "impute daily"
    strategy = ifelse(strategy == "delete", "impute summary",
                      ifelse(strategy == "impute", "impute daily",
                             strategy)),
    ## Calculate bias
    bias = est_beta - true_beta,
    ## Calculate CI width
    ci_width = ucl - lcl,
    ## Indicators for power, coverage
    power = pvalue < 0.05,
    coverage = true_beta >= lcl & true_beta <= ucl
  )

sim_results_grouped <- sim_results2 %>%
  group_by(miss_type, miss_prop, assoc, strategy, true_beta) %>%
  summarise_at(
    vars(bias, se_beta, ci_width, power, coverage), mean
  ) %>%
  ungroup() %>%
  ## Factors for plotting prettily
  mutate(
    strategy = factor(
      case_when(
        strategy == "ignore"         ~ 1,
        strategy == "worst"          ~ 2,
        strategy == "impute summary" ~ 3,
        strategy == "impute daily"   ~ 4,
        TRUE ~ as.numeric(NA)
      ),
      levels = 1:4,
      labels = c(
        "NA = unexposed",
        "NA = exposed",
        "Impute summary",
        "Impute daily"
      )
    ),
    assoc = factor(
      case_when(
        assoc == "weak"   ~ 1,
        assoc == "mod"    ~ 2,
        assoc == "strong" ~ 3,
        is.na(assoc)      ~ 4,
        TRUE ~ as.numeric(NA)
      ),
      levels = 1:4, labels = c("Weak", "Moderate", "Strong", " ")
    ),
    miss_type = factor(
      case_when(
        miss_type == "mcar" ~ 1,
        miss_type == "mar"  ~ 2,
        miss_type == "mnar" ~ 3,
        TRUE ~ as.numeric(NA)
      ),
      levels = 1:3, labels = c("MCAR", "MAR", "MNAR")
    ),
    miss_prop_f = fct_reorder(
      paste0(miss_prop * 100, "%"), miss_prop
    )
  )

# ## Create long format for faceted plotting
# sim_results_long <- sim_results_grouped %>%
#   gather(key = quantity_name, value = quantity_value, bias:coverage) %>%
#   ## Create proper factors for plotting
#   mutate(
#     quantity_f = factor(
#       case_when(
#         quantity_name == "bias"     ~ 1,
#         quantity_name == "se_beta"  ~ 2,
#         quantity_name == "ci_width" ~ 3,
#         quantity_name == "coverage" ~ 4,
#         quantity_name == "power"    ~ 5,
#         TRUE ~ as.numeric(NA)
#       ),
#       levels = 1:5,
#       labels = c("Bias", "Std Error", "CI Width", "Coverage", "Power")
#     ),
#     prop_f = paste0(round(miss_prop * 100), "% Missing"),
#     prop_f = fct_reorder(prop_f, miss_prop),
#     assoc = factor(
#       case_when(
#         assoc == "weak"   ~ 1,
#         assoc == "mod"    ~ 2,
#         assoc == "strong" ~ 3,
#         TRUE              ~ as.numeric(NA)
#       ),
#       levels = 1:3, labels = c("Weak", "Moderate", "Strong")
#     ),
#     strategy = factor(
#       case_when(
#         strategy == "ignore" ~ 1,
#         strategy == "worst" ~ 2,
#         strategy == "impute summary" ~ 3,
#         strategy == "impute daily" ~ 4,
#         TRUE ~ as.numeric(NA)
#       ),
#       levels = 1:4,
#       labels = c(
#         "Ignore (NA = no exp)", "Worst (NA = exp)",
#         "Impute summary value", "Impute daily value"
#       )
#     )
#   )

facet_breaks <- function(x){
  round(seq(min(x) - 0.05, max(x) + 0.05, length.out = 3), 0.05)
}

plot_measure <- function(
  measure,
  measure_title,
  # missingness = c("MCAR", "MAR", "MNAR"),
  beta_max = -1,
  beta_breaks = c(0, 0.4, 0.8, 1),
  df = sim_results_grouped
){
  ## Values for reference line, if applicable
  ref_value <- case_when(
    measure == "bias"     ~ 0,
    measure == "coverage" ~ 0.95,
    measure == "power"    ~ 0.05,
    TRUE                  ~ as.numeric(NA)
  )
  
  ## Leave true_beta > beta_max out to help focus on smaller effect sizes
  ## (remember, betas are negative)
  df_sub <- subset(df, true_beta >= beta_max)
  
  p <- ggplot(df_sub) +
    aes_string(x = "abs(true_beta)", y = measure, colour = "strategy")
  
  ## If measure is power or coverage, should always go from 0-1; otherwise,
  ##  y axis limits can vary
  if(measure %in% c("power", "coverage")){
    p <- p +
      facet_grid(miss_prop_f ~ miss_type + assoc) +
      scale_y_continuous(limits = c(0, 1), breaks = c(0, 0.5, 1), name = "")
  } else{
    p <- p +
      facet_grid(miss_prop_f ~ miss_type + assoc, scales = "free_y") +
      scale_y_continuous(name = "")
  }
  
  if(!is.na(ref_value)){
    p <- p +
      geom_hline(
        yintercept = ref_value, linetype = "dotted", colour = "#FFFFFF", size = 1
      )
  }
  
  p <- p +
    geom_line() +
    geom_point(size = 3.5, alpha = 0.8) +
    scale_x_continuous(
      name = "\nTrue Effect Size\n",
      breaks = beta_breaks,
      labels = beta_breaks * -1
    ) +
    scale_colour_viridis_d(name = "", direction = -1) +
    labs(title = paste0("\n", measure_title)) + #, y = "") +
    theme_dark(base_family = "Roboto Condensed") +
    theme(
      plot.title = element_text(face = "bold", size = 30, colour = "#FFFFFF"),
      plot.background = element_rect(fill = "#272822"),
      axis.title.x = element_text(
        colour = "#FFFFFF", size = 20, face = "bold", vjust = 0
      ),
      # axis.title.y = element_blank(),
      axis.text = element_text(colour = "#FFFFFF", size = 12),
      # legend.position = "none",
      legend.position = "top",
      legend.justification = c(1, 0.95),
      legend.direction = "horizontal",
      legend.background = element_rect(fill = "#272822"),
      legend.title = element_text(colour = "#FFFFFF", size = 15),
      legend.text = element_text(colour = "#FFFFFF", size = 15),
      strip.text.x = element_text(face = "bold", size = 20, hjust = 0),
      strip.text.y = element_text(face = "bold", size = 20, angle = 0),
      # panel.background = element_blank(),
      panel.spacing.x = unit(c(1.1, rep(0.25, 2), 1.1, rep(0.25, 2)), "lines"),
      panel.spacing.y = unit(0.5, "cm"),
      panel.grid = element_line(colour = "grey60")
    )
  
  return(p)
}

jsm_measures <- c("bias", "se_beta", "ci_width", "coverage", "power")
jsm_plots <- pmap(
  .l = list(
    measure = jsm_measures,
    measure_title = c("BIAS", "STANDARD ERROR", "CI WIDTH", "COVERAGE", "POWER")
  ),
  .f = plot_measure
) %>%
  set_names(jsm_measures)

jsm_plots_no3 <- pmap(
  .l = list(
    measure = jsm_measures,
    measure_title = c("BIAS", "STANDARD ERROR", "CI WIDTH", "COVERAGE", "POWER")
  ),
  .f = plot_measure,
  df = subset(sim_results_grouped, strategy != "Impute summary")
) %>%
  set_names(jsm_measures)

# ## Patchwork attempt: Way to get background to be something other than white?
# ## Will need wifi to investigate
# jsm_plots <- pmap(
#   .l = list(
#     measure = rep(jsm_measures, each = 3),
#     measure_title = rep(
#       c("BIAS", "STANDARD ERROR", "CI WIDTH", "COVERAGE", "POWER"),
#       each = 3
#     )#,
#     # missingness = rep(c("MCAR", "MAR", "MNAR"), times = length(jsm_measures))
#   ),
#   .f = plot_measure
# ) %>%
#   set_names(jsm_measures)
#   # set_names(
#   #   paste(
#   #     rep(jsm_measures, each = 3), rep(c("mcar", "mar", "mnar"), times = 3),
#   #     sep = "_"
#   #   )
#   # )
# 
# ## -- For each measure, patchwork together and save ----------------------------
# bias_plots <- jsm_plots[str_subset(names(jsm_plots), "^bias")]
# 
# ggsave(
#   filename = "testpatch.png",
#   bias_plots[[1]] + bias_plots[[2]] + bias_plots[[3]],
#   device = "png", path = "presfigures",
#   width = 10.67, height = 8, units = "in", dpi = 400
# )

walk(
  jsm_measures,
  ~ ggsave(
      filename = sprintf("%s.png", .),
      jsm_plots[[.]],
      path = "presfigures",
      device = "png",
      width = 10.67, height = 8, units = "in", dpi = 400
  )
)

walk(
  jsm_measures,
  ~ ggsave(
    filename = sprintf("%s_no3.png", .),
    jsm_plots_no3[[.]],
    path = "presfigures",
    device = "png",
    width = 10.67, height = 8, units = "in", dpi = 400
  )
)

## -- "Real world" analysis ----------------------------------------------------
rw_results <- readRDS("../results/realworld.rds") %>%
  mutate(
    strategy = factor(
      strategy, levels = c("NA = unexposed", "NA = exposed", "Impute daily")
    )
  )

rw_plot <- ggplot(data = rw_results, aes(x = fct_rev(strategy), y = est_beta)) +
  facet_wrap(~ study, nrow = 1, scales = "free_x") +
  geom_pointrange(aes(ymin = lcl, ymax = ucl, colour = strategy), size = 1) +
  scale_colour_manual(
    values = as.character(vcolors[c("yellow", "green", "purple")]),
    guide = "none"
  ) +
  coord_flip() +
  scale_y_continuous(name = "Estimated Effect\n(95% CI)") +
  theme_dark(base_family = "Roboto Condensed") +
  theme(
    axis.title.x = element_text(size = 20, face = "bold", vjust = 0),
    axis.title.y = element_blank(),
    axis.text = element_text(size = 15),
    strip.text = element_text(face = "bold", size = 20)
  )

ggsave(
  filename = "realworld.png",
  rw_plot,
  path = "presfigures",
  device = "png",
  # type = "cairo",
  width = 7, height = 5, units = "in", dpi = 400
)