# source data simulation
# which loads the packages
source('scripts/01-simulate-data.R')

# fits are written in to disk
# in the folder fits
# in 100% fresh installs
# there should not be any fits folder
# create it if so
if (!dir.exists('fits')) {
  dir.create('fits')
}

# ----general-consideration----
# all models fitted here have the exact same structure
# the only thing that changes is the response variable

# ----fit-cog----
# the response variable is the raw cog
# assign model formula
# to get a grid with all the priors
mdl.formula <- bf(
  cog ~ 0 + twn:g:sib +
    twn:g:sib:cl_age +
    (0 + sib | gr(sp, by = interaction(twn, g))) +
    (0 + twn:g | gr(item, by = sib))
  # sigma varies by sibilant
  , sigma ~ 0 + sib
)

# get all the priors needed by
# the previous formula and the data frame
# to change manually
pr <- get_prior(
  mdl.formula
  , data = df
)

# assign custom priors
custom.pr <- pr %>% 
  mutate(
    prior = case_when(
      # correlations
      class == 'cor' ~ 'lkj(2)'
      # group-level adjustments
      , class == 'sd' ~ 'normal(40, 100)'
      # sigma
      , dpar == 'sigma' ~ 'normal(400, 150)'
      # betas
      , dpar != 'sigma' & class == 'b' &
        grepl(
          'age'
          , coef
        ) ~ 'normal(0, 1050)'
        # alphas
      , dpar != 'sigma' & class == 'b' &
        !grepl(
          'age'
          , coef
        ) ~ 'normal(5000, 1000)'
    )
  )

# fit model
fit.cog <- brm(
  mdl.formula
  , data = df
  , prior = custom.pr
  , file = 'fits/01-cog'
  , thin = 2
  , seed = 20
)

# ----fit-cog-summary----
# summary(fit.cog)

# ----res-fit-cog-cmeans----

# pop.post <- df %>% 
#   distinct(
#     twn, g
#     , sib
#     , cl_age
#   ) %>% 
#   add_epred_draws(
#     object = fit.cog
#     , value = 'cog'
#     , re_formula = NA
#   )
# 
# pop.post %>% 
#   ggplot(
#     aes(
#       x = cl_age
#       , y = cog
#       , color = g
#       , fill = g
#     )
#   ) +
#   stat_lineribbon(
#     .width = c(.5, .89)
#     , alpha = .2
#   ) +
#   geom_point(
#     data = df %>% 
#       group_by(
#         sp, cl_age
#         , g, twn, sib
#       ) %>% 
#       summarise(cog = mean(cog))
#   ) +
#   facet_wrap(
#     ~twn*sib
#   )

# ----fit-z----
# the response variable is the cog standardised

# create standardised cog
df$cog_z <- (df$cog - mean(df$cog)) / sd(df$cog)

# same model as before
# different response
mdl.formula <- bf(
  cog_z ~ 0 + twn:g:sib +
    twn:g:sib:cl_age +
    (0 + sib | gr(sp, by = interaction(twn, g))) +
    (0 + twn:g | gr(item, by = sib))
  , sigma ~ 0 + sib
)

# get priors
pr <- get_prior(
  mdl.formula
  , data = df
)

# assign priors
custom.pr <- pr %>% 
  mutate(
    prior = case_when(
      class == 'cor' ~ 'lkj(2)'
      , class == 'sd' ~ 'normal(0, 0.5)'
      , dpar == 'sigma' ~ 'normal(0, 1)'
      , dpar != 'sigma' & class == 'b' &
        grepl(
          'age'
          , coef
        ) ~ 'normal(0, 2)'
      , dpar != 'sigma' & class == 'b' &
        !grepl(
          'age'
          , coef
        ) ~ 'normal(0, 2)'
    )
  )

# fit model
# this is much faster than the previous
fit.z <- brm(
  mdl.formula
  , data = df
  , prior = custom.pr
  , file = 'fits/02-z'
  , thin = 2
  , seed = 20
)

# ----fit-z-summary----
# summary(fit.z)

# ----res-fit-z-cmeans----

# pop.post <- df %>% 
#   distinct(
#     twn, g
#     , sib
#     , cl_age
#   ) %>% 
#   add_epred_draws(
#     object = fit.z
#     , value = 'cog_z'
#     , re_formula = NA
#   )
# 
# pop.post %>% 
#   ggplot(
#     aes(
#       x = cl_age
#       , y = cog_z
#       , color = g
#       , fill = g
#     )
#   ) +
#   stat_lineribbon(
#     .width = c(.5, .89)
#     , alpha = .2
#   ) +
#   geom_point(
#     data = df %>% 
#       group_by(
#         sp, cl_age
#         , g, twn, sib
#       ) %>% 
#       summarise(cog_z = mean(cog_z))
#   ) +
#   facet_wrap(
#     ~twn*sib
#   )

# ----fit-lob----
# the response variable is the lobanov cog

# create lobanov transformed cog
# grouped by speaker
df <- df %>% 
  group_by(
    sp
  ) %>% 
  mutate(
    lob_cog = (cog - mean(cog)) / sd(cog)
  )

# same formula, response lobanov
mdl.formula <- bf(
  lob_cog ~ 0 + twn:g:sib +
    twn:g:sib:cl_age +
    (0 + sib | gr(sp, by = interaction(twn, g))) +
    (0 + twn:g | gr(item, by = sib))
  , sigma ~ 0 + sib
)

# get priors
pr <- get_prior(
  mdl.formula
  , data = df
)

# assign priors
custom.pr <- pr %>% 
  mutate(
    prior = case_when(
      class == 'cor' ~ 'lkj(2)'
      , class == 'sd' ~ 'normal(0, .5)'
      , dpar == 'sigma' ~ 'normal(0, .5)'
      , dpar != 'sigma' & class == 'b' &
        grepl(
          'age'
          , coef
        ) ~ 'normal(0, 1)'
      , dpar != 'sigma' & class == 'b' &
        !grepl(
          'age'
          , coef
        ) ~ 'normal(0, 2)'
    )
  )

# fit model
fit.lob <- brm(
  mdl.formula
  , data = df
  , prior = custom.pr
  , file = 'fits/03-lob-cog'
  , thin = 2
  , seed = 20
)

# ----fit-lob-summary----
# summary(fit.lob)


# ----store-fit-objects----

fit.objects <- ls(pattern = '^fit.')

posterior.cols <- colnames(
  as_draws_df(
    get(fit.objects[1])
  )
)


# ----clear-garbage----
rm(list = c('pr', 'custom.pr'))
