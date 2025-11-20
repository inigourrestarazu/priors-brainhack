# ----basic-packages----

library(magrittr) # pipe operator
library(dplyr) # easy manipulation
library(tidyr)


# ----ggplot----
library(ggplot2) # plot
library(ggdist) # plot distributions
library(geomtextpath) # text to plots
library(patchwork) # combine plots
library(viridis)

# set basic theme
theme_set(
  theme_classic( # theme
    base_size = 22 # size of non-graph
    , base_family = 'Carlito' # font
  )
)

# ----tables----
library(gt)
library(gtExtras)
library(kableExtra)

# ----lme4----
library(lme4)
library(broom)
library(broom.mixed)

# ----helpers
library(emmeans)
library(marginaleffects)

# ----brms----
# load package
library(brms)
# auto-parallel
# use as many cores as chains
# if ncores <= nchains
options(mc.cores = parallel::detectCores())
# use threading (split chains)
rstan::rstan_options(threads_per_chain = 2)
# write stan code to temp file
rstan::rstan_options(auto_write = TRUE)
# use cmdstanr backend
# it's much faster
options(brms.backend = "cmdstanr")
# call it explicitly
# so that it's cited directly
# otherwise it is used under the hood
# but grateful does not pick it
library(cmdstanr)
# seed for models
# ensure exact replication
