
rm(list = ls()) # clear the environment

# packages ----------------------------------------------------------------

library(tidyverse)
library(coolorrr)
coolorrr::set_theme()
set_palette()
library(mgcv)
library(modelsummary)
library(sjPlot)
library(patchwork)


# data --------------------------------------------------------------------

# read in the data
dt <- read_rds(
  here::here("03_data", "williams_final_data.rds")
)

# only keep country years for the same data
# between MIDs and foreign aid
# dt <- drop_na(dt, aid)

dt <- dt |>
  group_by(year) |>
  mutate(
    power = log(exp(sdpest) / sum(exp(sdpest), na.rm = T) * percent_win),
    sdpdis = log(expectations) - power
  )


# only keep years > 1972
dt <- filter(dt, year > 1972)

dt <- dt |>
  group_by(year) |>
  mutate(
    other_mids = gmlmidonset * (1 - gmlmidonset_init),
    other_aid  = log((sum(aid, na.rm = T) - aid) / 
      (n() - 1))
  )

# summary stats -----------------------------------------------------------

# na.rm = T versions of functions
meanna   <- function(x) mean(x, na.rm = T)
medianna <- function(x) median(x, na.rm = T)
sdna     <- function(x) sd(x, na.rm = T)
minna    <- function(x) min(x, na.rm = T)
maxna    <- function(x) max(x, na.rm = T)

datasummary(
  data = dt,
  formula = gmlmidonset_init + aid + kappavv + percent_win +
    dissatisfaction ~ meanna + medianna + sdna + minna + maxna,
  na.rm = T
)

# the deets
deets <- function(data, drop.na.by) {
  ndt <- drop_na(data, !!enquo(drop.na.by))
  c(
    N = nrow(ndt),
    unique.obs = length(unique(ndt$ccode)),
    years = paste0(range(ndt$year), 
                   collapse = "--")
  )
}

deets(dt, gmlmidonset_init)
deets(dt, aid)

# regression analysis -----------------------------------------------------

## Fit the models ----
gam(
  gmlmidonset_init ~ 
    percent_win + 
    dissatisfaction +
    kappavv +
    wbgdppc2011est +
    poly(gmlmidinitspell, 3) +
    s(ccode, bs = "re"),
  data = dt,
  family = binomial
) -> logitfit
gam(
  aid ~ 
    percent_win + 
    dissatisfaction +
    kappavv +
    wbgdppc2011est +
    poly(year, 3) +
    s(ccode, bs = "re"),
  data = dt,
  family = quasipoisson
) -> ppmlfit

## Summarize in coefplot ----
cm <- c(
  "percent_win"     = "Power",
  "kappavv"         = "Alignment",
  "dissatisfaction" = "Dissatisfaction",
  "wbgdppc2011est"  = "GDP/capita"
)
f <- function(x) format(round(x, 3), big.mark=",")
gm <- list(
  list("raw" = "nobs", "clean" = "N", "fmt" = f))
modelplot(
  models = list(
    "MID Initiation (Logit)" = logitfit,
    "Foreign Aid (PPML)" = ppmlfit
  ),
  # vcov = "HC0",
  # cluster = "ccode",
  stars = T,
  coef_map = cm,
  gof_map = gm,
  color = "steelblue",
  background = list(
    geom_vline(xintercept = 0, lty = 2)
  )
) +
  facet_wrap(~ model, scales = "free_x") +
  geom_text(
    aes(x = estimate,
        y = term,
        label = paste0(
          round(estimate, 3),
          gtools::stars.pval(p.value),
          "\n(", round(std.error, 3), ")")),
    vjust = -.3,
    size = 3
  ) +
  labs(
    subtitle = str_wrap(
      "Note: .p < 0.1, *p < 0.05, **p < 0.01, ***p < 0.001. Regression estimates with (standard errors) shown above the point estimates. Models include random country slopes and cubic trends (not shown).",
      width = 80
    ),
    x = "Coefficient with 95% CIs",
    caption = str_wrap(
      "Figure 2: Regression model estimates with 95% confidence intervals.",
      width = 80
    )
  ) +
  theme(
    plot.caption = element_text(
      hjust = 0,
      size = 12
    ),
    plot.caption.position = "plot",
    plot.subtitle = element_text(
      size = 10
    ),
    plot.title.position = "plot",
    axis.title = element_text(
      hjust = .5
    )
  )
ggsave(
  here::here(
    "02_report",
    "figs",
    "modelplot.png"
  ),
  height = 5,
  width = 6
)

## Visualize marginal effects ----
## For MIDs
plot_model(
  logitfit,
  type = "pred",
  term = "percent_win",
  vcov.fun = vcovCL(
    logitfit,
    cluster = dt$ccode,
    type = "HC0"
  )
) + 
  labs(
    x = expression("Power"%->%""),
    y = "Pr. MID Initiation"
  ) -> p1
plot_model(
  logitfit,
  type = "pred",
  term = "dissatisfaction",
  vcov.fun = vcovCL(
    logitfit,
    cluster = dt$ccode,
    type = "HC0"
  )
) + 
  labs(
    x = expression("Dissatisfaction"%->%""),
    y = "Pr. MID Initiation"
  ) -> p2
plot_model(
  logitfit,
  type = "pred",
  term = "kappavv",
  vcov.fun = vcovCL(
    logitfit,
    cluster = dt$ccode,
    type = "HC0"
  )
) + 
  labs(
    x = expression("Alignment"%->%""),
    y = "Pr. MID Initiation"
  ) -> p3

## For aid
rescalefun <- function(x) {
  paste0(scales::dollar(x / 1e09), " bn")
}
plot_model(
  ppmlfit,
  type = "pred",
  term = "percent_win",
  vcov.fun = vcovCL(
    logitfit,
    cluster = dt$ccode,
    type = "HC0"
  )
) + 
  scale_y_continuous(
    labels = rescalefun
  ) +
  labs(
    x = expression("Power"%->%""),
    y = "Foreign Aid"
  ) -> p4
plot_model(
  ppmlfit,
  type = "pred",
  term = "dissatisfaction",
  vcov.fun = vcovCL(
    logitfit,
    cluster = dt$ccode,
    type = "HC0"
  )
) + 
  scale_y_continuous(
    labels = rescalefun
  ) +
  labs(
    x = expression("Dissatisfaction"%->%""),
    y = "Foreign Aid"
  ) -> p5
plot_model(
  ppmlfit,
  type = "pred",
  term = "kappavv",
  vcov.fun = vcovCL(
    logitfit,
    cluster = dt$ccode,
    type = "HC0"
  )
) + 
  scale_y_continuous(
    labels = rescalefun
  ) +
  labs(
    x = expression("Alignment"%->%""),
    y = "Foreign Aid"
  ) -> p6

col_label_1 <- wrap_elements(
  panel = grid::textGrob('(MID Initiation)')
)
col_label_2 <- wrap_elements(
  panel = grid::textGrob('(Aid Commitments)')
)

coolorrr::set_theme()
col_label_1 + col_label_2 + p1 + p4 + p2 + p5 + p3 + p6 +
  plot_annotation(
    caption = str_wrap(
      "Figure 3: Simulated model predictions for each predictor variable and outcome.",
      width = 75
    )
  ) +
  plot_layout(
    ncol = 2,
    heights = c(.1, 1, 1, 1)
  ) &
  labs(
    title = NULL,
    y = NULL
  ) &
  theme(
    axis.title.x = element_text(
      face = "italic",
      hjust = .5
    ),
    plot.caption = element_text(
      size = 12,
      hjust = 0
    ),
    plot.caption.position = "plot"
  )

ggsave(
  here::here(
    "02_report",
    "figs",
    "model_predictions.png"
  ),
  height = 8,
  width = 6,
  dpi = 500
)
