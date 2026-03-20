
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
  here::here("03_data", "williams_final_data[ADDMICS].rds")
)

dt <- dt |>
  group_by(year) |>
  mutate(
    power = (exp(sdpest) / sum(exp(sdpest), na.rm = T))
  )

## post ww2 data

dt <- filter(dt, year > 1945)


# summary stats -----------------------------------------------------------

# na.rm = T versions of functions
meanna   <- function(x) mean(x, na.rm = T)
medianna <- function(x) median(x, na.rm = T)
sdna     <- function(x) sd(x, na.rm = T)
minna    <- function(x) min(x, na.rm = T)
maxna    <- function(x) max(x, na.rm = T)

datasummary(
  data = dt,
  formula = miconset_init + oda_committed + kappavv + power +
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

deets(dt |> filter(year < 2015), miconset_init)
deets(dt, oda_committed)
deets(dt, kappavv)
deets(dt, power)
deets(dt, dissatisfaction)

# regression analysis -----------------------------------------------------

## Fit the models ----

### Modeling change seeking foreign policies:
gam(
  miconset_init ~ 
    power + 
    dissatisfaction +
    kappavv +
    poly(micspell, 3) +
    s(ccode, bs = "re"),
  data = dt |> filter(year < 2015),
  family = binomial
) -> init_fit
gam(
  oda_committed ~ 
    power + 
    dissatisfaction +
    kappavv +
    poly(year, 3) +
    s(ccode, bs = "re"),
  data = dt,
  family = quasipoisson
) -> aid_fit

## Summarize in coefplot ----
cm <- c(
  "power"     = "Power",
  "kappavv"         = "Alignment",
  "dissatisfaction" = "Dissatisfaction"
)
f <- function(x) format(round(x, 3), big.mark=",")
gm <- list(
  list("raw" = "nobs", "clean" = "N", "fmt" = f))
modelplot(
  models = list(
    "MIC Initiation (Logit)" = init_fit,
    "Foreign Aid (PPML)" = aid_fit
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
  facet_wrap(~ model, scales = "free_x", ncol = 2) +
  geom_text(
    aes(x = estimate,
        y = term,
        label = paste0(
          round(estimate, 3),
          gtools::stars.pval(p.value))),
    vjust = -1,
    size = 3
  ) +
  labs(
    # subtitle = str_wrap(
    #   "Note: .p < 0.1, *p < 0.05, **p < 0.01, ***p < 0.001.",
    #   width = 150/1.5
    # ),
    x = "Coefficient with 95% CIs"
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
  height = 4/1.5,
  width = 8/1.5
)

library(ggregtab)

bind_rows(
  tidy_coeftest(
    init_fit,
    model = "MIC Initiations (Logit)"
  ),
  tidy_coeftest(
    aid_fit,
    model = "ODA Commitments (PPML)"
  )
) |>
  filter(
    term %in% c(
      "power",
      "dissatisfaction",
      "kappavv"
    )
  ) |>
  ggregtab(ratio = .3) +
  labs(
    title = str_wrap(
      "Table 2: Regression model estimates"
    )
  ) +
  scale_y_discrete(
    labels = c(
      "Alignment",
      "Dissatisfaction",
      "Power"
    )
  )

ggsave(
  here::here(
    "02_report",
    "figs",
    "regtab.png"
  ),
  height = 3,
  width = 6
)

## Visualize marginal effects ----
## For MIDs
plot_model(
  init_fit,
  type = "pred",
  term = "power",
  vcov.fun = vcovCL(
    logitfit,
    cluster = dt$ccode,
    type = "HC0"
  )
) + 
  labs(
    x = expression("Power"%->%""),
    y = NULL,
    title = NULL
  ) -> p1
plot_model(
  init_fit,
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
    y = NULL,
    title = NULL
  ) -> p2
plot_model(
  init_fit,
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
    y = NULL,
    title = NULL
  ) -> p3

## For aid
rescalefun <- function(x) {
  paste0(scales::dollar(x / 1e03), " bn")
}
plot_model(
  aid_fit,
  type = "pred",
  term = "power",
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
    y = NULL,
    title = NULL
  ) -> p4
plot_model(
  aid_fit,
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
    y = NULL,
    title = NULL
  ) -> p5
plot_model(
  aid_fit,
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
    y = NULL,
    title = NULL
  ) -> p6

col_label_1 <- wrap_elements(
  panel = grid::textGrob('(MIC Initiation)')
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


