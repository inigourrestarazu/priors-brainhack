
# ----general-purpose----
library(magrittr)
library(dplyr)
library(tidyr)
library(forcats)

# ----ggplot----
library(ggplot2)
library(ggdist)
library(geomtextpath)
library(patchwork)
theme_set(
  theme_classic(
    base_size = 16
    , base_family = 'Times'
    )
  )

# colors for plots
colors.fit <- RColorBrewer::brewer.pal(3, 'Set1')
names(colors.fit) <- c('cog', 'lob', 'z')
labels.fit <- c(
  'Untransformed CoG'
  , 'Standardised groupwise'
  , 'Standardised'
  )


# ----brms----
library(brms)
options(mc.cores = parallel::detectCores())
rstan::rstan_options(threads_per_chain = 2)
rstan::rstan_options(auto_write = TRUE)
options(brms.backend = "cmdstanr")
library(tidybayes)
library(cmdstanr) # to force citation by grateful
