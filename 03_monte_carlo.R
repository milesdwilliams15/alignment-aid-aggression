
rm(list = ls()) # clean environment


# Packages ----------------------------------------------------------------

library(tidyverse)
library(patchwork)
library(coolorrr)
coolorrr::set_theme()
set_palette()

# Helper functions --------------------------------------------------------

fci <- function(bi, bj, cj, aaij, ddi) {
  #ci <- bi * ddi * (1 - mean(cj / bj)) - (1 - ddi) * mean(aaij * cj)
  ci <- bi * ddi * (1 + mean((aaij - 1 / bj)*cj)) - mean(aaij * cj)
  pmax(0, pmin(bi, ci))
}

findNash <- function(b, aa, dd, maxit = 1000, gran = 8) {
  N  <- length(b)
  cs <- matrix(
    0L, 
    nrow = maxit + 1, 
    ncol = N
  )
  if(N != nrow(aa) | N != length(dd)) {
    stop("Incorrect no. of dims.")
  }
  for(it in 1:maxit) {
    for(i in 1:N) {
      cs[it + 1, i] <- fci(
        bi = b[i], 
        bj = b[-i], 
        cj = cs[it, -i],
        aaij = aa[i, -i], 
        ddi = dd[i]
      )
    }
    eqFound <- all(round(apply(cs[c(it, it + 1), ], 2, diff), gran) == 0)
    if(eqFound) break
  }
  cs <- cs[2:it, ] |>
    as.data.frame()
  colnames(cs) <- paste0("c", 1:N)
  cs$t <- 1:(it- 1)
  cs
}

plotNash <- function(...) {
  fn <- findNash(...)
  fn |>
    pivot_longer(
      starts_with("c")
    ) |>
    ggplot() +
    aes(x = t, y = value, color = name) +
    geom_point() +
    geom_line()
}

reportNash <- function(...) {
  fn <- findNash(...)
  fn[nrow(fn), ]
}

mcNash <- function(N = 5, its = 999, maxit = 99, gran = 8) {
  mc <- list()
  for(i in 1:its) {
    # Define distribution of resources
    b <- runif(N, .1, .9); b <- b / sum(b)
    
    # Define each country's disatisfaction
    dd <- runif(N, .1, .9)
    
    # Define alignment## Create an empty matrix filled with zeros
    aa <- matrix(1, nrow = N, ncol = N)
    
    ## Fill the upper triangle (including the diagonal) with 1s
    aa[upper.tri(aa, diag = F)] <- runif(
      N * (N - 1) / 2, -.9, .9
    )
    
    ## Mirror the upper triangle to the lower triangle
    aa[lower.tri(aa)] <- t(aa)[lower.tri(aa)]
    
    # Find Nash Equilibrium
    rn <- reportNash(b, aa, dd, maxit, gran)
    
    # # Report system-level aggregates
    # mc[[i]] <- tibble(
    #   total_c = rowSums(rn[ , 1:N]),
    #   b   = -sqrt(sum((b - 1/N)^2)),
    #   a   = mean(aa[upper.tri(aa)]),
    #   d   = mean(dd)
    # )
    
    # Report country-level aggregates
    avg_a <- 0
    for(n in 1:N) {
      avg_a[n] <- aa[n, -n] |> mean()
    }
    mc[[i]] <- tibble(
      it = i,
      i  = 1:N,
      ci = c(rn[, 1:N]),
      b  = b,
      d  = dd,
      a  = avg_a
    ) |> unnest(ci)
  }
  do.call("rbind", mc)
}

partial <- function(...) {
  lm(...) |> resid()
}

# Monte Carlo analysis ----------------------------------------------------

## Simulate an international system with:
## * N = 10 states
## * i = 10000 times
set.seed(444)
nashdt <- mcNash(N = 10, its = 10000, gran = 3) 

## Visualize the relationships

### The partial relationship
### for a and ci:
nashdt |>
  mutate(
    ci = partial(ci ~ b + d),
    a  = partial(a  ~ b + d)
  ) |>
  ggplot() +
  aes(x = a, y = ci) +
  geom_point(
    col = "gray",
    alpha = 0.1
  ) +
  geom_smooth(
    method = "lm",
    se = F,
    col = "steelblue"
  ) -> p1

### The partial relationship
### for b and ci:
nashdt |>
  mutate(
    ci = partial(ci ~ a + d),
    b  = partial(b  ~ a + d)
  ) |>
  ggplot() +
  aes(x = b, y = ci) +
  geom_point(
    col = "gray",
    alpha = 0.1
  ) +
  geom_smooth(
    method = "lm",
    se = F,
    col = "steelblue"
  ) -> p2

### The partial relationship
### for d and ci:
nashdt |>
  mutate(
    ci = partial(ci ~ a + b),
    d  = partial(d  ~ a + b)
  ) |>
  ggplot() +
  aes(x = d, y = ci) +
  geom_point(
    col = "gray",
    alpha = 0.1
  ) +
  geom_smooth(
    method = "lm",
    se = F,
    col = "steelblue"
  ) -> p3

### Combine as a single plot
(p1 + labs(subtitle = "...Alignment")) +
  (p2 + labs(subtitle = "...Power")) +
  (p3 + labs(subtitle = "...Dissatisfaction")) +
  plot_layout(
    ncol = 1
  ) +
  plot_annotation(
    title = "Impact on change-seeking by...",
    caption = str_wrap(
      "Figure 1: Partial prediction plots from a regression model fit to Monte Carlo data drawn from the data-generating process implied by the theoretical model.",
      width = 75
    )
  ) &
  theme(
    axis.text = element_blank(),
    panel.border = element_rect(
      color = "gray",
      linetype = 1
    ),
    plot.caption = element_text(
      hjust = 0,
      size = 12
    ),
    plot.caption.position = "plot"
  ) &
  labs(
    x = NULL,
    y = NULL
  )

ggsave(
  here::here(
    "02_report",
    "figs",
    "monte_carlo_partials.png"
  ),
  height = 8,
  width = 6,
  dpi = 500
)
