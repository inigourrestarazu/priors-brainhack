source('scripts/00-load-packages.R')

# This script simulates some data
# from a normal distribution,
# that should resemble the Center of Gravity
# of Basque fricative sibilants, that vary as a function of
# gender, town, and age.
# There are repeated measures by
# speaker and item.
# Each speaker has a deviation from the mean
# for each sibilant.
# At the same time,
# the variances and correlations vary
# by combination of town and gender.
# Items have different adjustments
# for each combination of town and gender,
# and correlations and variances vary by sibilant.
# Finally, sibilants have different sd values,
# but, for simplicity, don't vary by any cluster

# ----data-simulation-seed----

set.seed(1)

# ----condition-grid----

# create a data frame with
# - the combinations of categorical variables,
# - their corresponding population parameters
# - their corresponding group-level sds
# used in the linear predictor

# gender
g <- c('man', 'woman')

# sibilants
sib <- c('z', 's', 'x')

# town
twn <- c('a', 'b')

# create a grid of combinations
cat.grid <- expand.grid(twn = twn, g = g, sib = sib)

# assign population-level averages at average age
cat.grid$alpha <- c(
  # first of each row, twn == a
  # z
  # man
  6000, 6300
  # woman
  , 6300, 6450
  # s
  # man
  , 5500, 5500
  # woman
  , 5600, 5700
  # x
  # man
  , 4500, 4550
  # woman
  , 4800, 4600
)

# assign slopes of **log age**
cat.grid$beta <- c(
  # first of each row, twn == a
  # z
  # man
  -400, -450
  # woman
  , -350, -350
  # s
  # man
  , -250, -260
  # woman
  , -250, -150
  # x
  # man
  , -100, -80
  # woman
  , -120, -100
)

# sd of sibilants
sigmas <- data.frame(
  sib
  , sigma = c(600, 500, 400)
)

cat.grid <- left_join(
  cat.grid, sigmas
)

# ----participant-adj-grid----

# create a data frame with
# - participant ids
# - ages
# - conditions
# - corresponding deviations from pop avg
# used to simulate observations
#
# 1. create a df with sp ids, age, twn and g
# 2. create group-level sds and correlations
# 3. loop over combinations of twn and g
#   # apparently, mvrnorm cannot be vectorised
#   # with multiple covariance matrices

# number of participants per condition
n.p <- 25

# vector of participant ids
sp.id <- paste0(
  'sp_', 1:(n.p * length(twn) * length(g))
)

# vector of ages
age <- sample(
  45:75
  , size = length(sp.id)
  , replace = T
)

# vectors of twn and gender
twns <- rep(twn, each = n.p * length(g))
gs <- rep(g, times = n.p * length(twn))

# combine previous vectors in a df
# so that each speaker has their own twn and g
sp.grid <- data.frame(
  sp = sp.id
  , age
  , twn = twns
  , g = gs
)

# arrange by twn and g
# in that order
# so that the adjustments simulated by mvrnorm
# belong to the correct group
sp.grid <- arrange(
  sp.grid
  , twn, g
)

# vector of means to simulate from the multivariate normal
# they must be 0,
# because 0 represents the population avg
# and these will be adjustments around the mean
mus <- setNames(rep(0, length(sib)), sib)


# sds vectors by combination of twn and g
# they represent the sd of the adjustments
# around the pop-mean for each sibilant
#
sds.twn_a.g_man <- setNames(
  c(150, 140, 130) 
  , sib
)

sds.twn_a.g_woman <- setNames(
  c(170, 160, 150) 
  , sib
)

sds.twn_b.g_man <- setNames(
  c(100, 90, 80) 
  , sib
)

sds.twn_b.g_woman <- setNames(
  c(150, 130, 120) 
  , sib
)

# vector of sds for adjustments
# to pass the to mvnorm within the loop
sds.twn.g <- ls(
  pattern = '^sds.twn_'
)

# correlation matrices
# for each combination of twn and g
cor.twn_a.g_man <- diag(length(sib))
# 1 == 'z'
# 2 == 's'
# 3 == 'x'
colnames(cor.twn_a.g_man) <- sib
rownames(cor.twn_a.g_man) <- sib
cor.twn_a.g_man[1, 2] <- cor.twn_a.g_man[2, 1] <- 0.5
cor.twn_a.g_man[1, 3] <- cor.twn_a.g_man[3, 1] <- -0.7
cor.twn_a.g_man[2, 3] <- cor.twn_a.g_man[3, 2] <- 0.4

cor.twn_a.g_woman <- diag(length(sib))
colnames(cor.twn_a.g_woman) <- sib
rownames(cor.twn_a.g_woman) <- sib
cor.twn_a.g_woman[1, 2] <- cor.twn_a.g_woman[2, 1] <- 0.8
cor.twn_a.g_woman[1, 3] <- cor.twn_a.g_woman[3, 1] <- -0.5
cor.twn_a.g_woman[2, 3] <- cor.twn_a.g_woman[3, 2] <- 0.6

cor.twn_b.g_man <- diag(length(sib))
colnames(cor.twn_b.g_man) <- sib
rownames(cor.twn_b.g_man) <- sib
cor.twn_b.g_man[1, 2] <- cor.twn_b.g_man[2, 1] <- 0.3
cor.twn_b.g_man[1, 3] <- cor.twn_b.g_man[3, 1] <- -0.7
cor.twn_b.g_man[2, 3] <- cor.twn_b.g_man[3, 2] <- 0.2

cor.twn_b.g_woman <- diag(length(sib))
colnames(cor.twn_b.g_woman) <- sib
rownames(cor.twn_b.g_woman) <- sib
cor.twn_b.g_woman[1, 2] <- cor.twn_b.g_woman[2, 1] <- 0.7
cor.twn_b.g_woman[1, 3] <- cor.twn_b.g_woman[3, 1] <- -0.4
cor.twn_b.g_woman[2, 3] <- cor.twn_b.g_woman[3, 2] <- 0.5

# store in a vector for the loop
cor.twn.g <- ls(pattern = '^cor.twn_')

# create and empty vector, to store the adjustments
sp.adjs <- matrix(
  ncol = length(sib)
  , nrow = length(sp.id)
)

# loop over combinations of twn and g
# to simulate the by-participant adjustments
# looping because covariance matrices cannot be
# passed vectorised, but one at a time
for (w in seq_len(length(cor.twn.g))) {
  
  # create covariance matrix
  # from sds and correlation matrix
  # for a given combination of twn and g
  Sigma <- diag(get(sds.twn.g[w])) %*%
    get(cor.twn.g[w]) %*% 
    diag(get(sds.twn.g[w]))
  
  # this is here to stabilise the cov matrix
  # apparently they tend to break,
  # specially if correlations are very high
  Sigma <- as.matrix(
    Matrix::nearPD(Sigma)$mat
  )
  
  # simulate adjustments with mvrnorm
  sp.adjs[
    (
      (length(sp.id) / length(cor.twn.g) * w) -
        length(sp.id) / length(cor.twn.g) + 1
    ):(
      length(sp.id) / length(cor.twn.g) * w
    )
    ,
  ] <- 
    MASS::mvrnorm(
      n = length(sp.id) / length(cor.twn.g)
      , mu = mus
      , Sigma = Sigma
    )
}

# assign column names to the adjustments
# so that they can be cbind-ed
# to the sp grid
colnames(sp.adjs) <- sib

# combine grid of speakers with their corresponding adjustments
sp.grid <- cbind(
  sp.grid
  , sp.adjs
)

# pivot longer
# to make the final simulation easier
# so that each observation only has its corresponding adjustments
# otherwise, which adjustment to use
# has to be selected with ifelses
sp.grid <- pivot_longer(
  sp.grid
  , cols = c('z', 's', 'x')
  , names_to = 'sib'
  , values_to = 'part.sib.adj'
)


# ----token-grid----

# create adjustment grid for item
# the logic is the reverse of speaker adjustments
# items can only belong to one sibilant
# but adjustments vary by twn and g combination

# number of tokens per sibilant
# i.e. 10 words containing each target sibilant
n.t.s <- 10

# vector of item ids
t.id <- paste(
  'item'
  , rep(sib, each = n.t.s)
  , 1:n.t.s
  , sep = '_'
)

# create a vector with the combinations of twn and g
# and sorthem
# so that assigning mus and sds is more consistent
t.g.combined <- expand.grid(twn, g)
t.g.combined <- sort(
  paste(
    t.g.combined[,1], t.g.combined[,2]
    , sep = ':'
  )
)

# mus must be 0
# because the adjustments are around the pop-mean
mus <- setNames(rep(0, length(t.g.combined)), t.g.combined)

# create vectors of sds for each combination of twn and g
# sds
sds.sib_z <- setNames(
  c(70, 80, 70, 70) 
  , t.g.combined
)

sds.sib_s <- setNames(
  c(50, 60, 50, 50) 
  , t.g.combined
)

sds.sib_x <- setNames(
  c(40, 50, 40, 40) 
  , t.g.combined
)

# store all in a vector
# ordered by sibilants from front to back
sds.sib <- ls(
  pattern = '^sds.sib_'
)[c(3, 1, 2)]

# correlation matrices between combinations of twn and g
cor.sib_z <- diag(length(t.g.combined))
rownames(cor.sib_z) <- t.g.combined
colnames(cor.sib_z) <- t.g.combined
cor.sib_z[1, 2] <- cor.sib_z[2, 1] <- 0.7
cor.sib_z[1, 3] <- cor.sib_z[3, 1] <- 0.4
cor.sib_z[2, 3] <- cor.sib_z[3, 2] <- -0.5
cor.sib_z[1, 4] <- cor.sib_z[4, 1] <- -0.5
cor.sib_z[2, 4] <- cor.sib_z[4, 2] <- 0.2
cor.sib_z[3, 4] <- cor.sib_z[4, 2] <- 0.5

cor.sib_s <- diag(length(t.g.combined))
rownames(cor.sib_s) <- t.g.combined
colnames(cor.sib_s) <- t.g.combined
cor.sib_s[1, 2] <- cor.sib_s[2, 1] <- 0.6
cor.sib_s[1, 3] <- cor.sib_s[3, 1] <- 0.3
cor.sib_s[2, 3] <- cor.sib_s[3, 2] <- -0.4
cor.sib_s[1, 4] <- cor.sib_s[4, 1] <- -0.4
cor.sib_s[2, 4] <- cor.sib_s[4, 2] <- 0.1
cor.sib_s[3, 4] <- cor.sib_s[4, 2] <- 0.4

cor.sib_x <- diag(length(t.g.combined))
rownames(cor.sib_x) <- t.g.combined
colnames(cor.sib_x) <- t.g.combined
cor.sib_x[1, 2] <- cor.sib_x[2, 1] <- 0.1
cor.sib_x[1, 3] <- cor.sib_x[3, 1] <- 0.1
cor.sib_x[2, 3] <- cor.sib_x[3, 2] <- -0.1
cor.sib_x[1, 4] <- cor.sib_x[4, 1] <- -0.1
cor.sib_x[2, 4] <- cor.sib_x[4, 2] <- 0.1
cor.sib_x[3, 4] <- cor.sib_x[4, 2] <- 0.1


# assign matrices to a vector
# to loop over in the sim. of the adjustments
cor.sib <- ls(pattern = '^cor.sib_')[
  c(3, 1, 2)
]

# create and empty matrix
# to store the adjustments that belong to each item
it.adjs <- matrix(
  ncol = length(t.g.combined)
  , nrow = length(t.id)
)

# simulate adjustments
# the logic is the same as with sp adjustments
# they're looped because mvrnorm cannot handle
# multiple covriance matrices at once
# loop over the number of sib(ilants)
for (w in seq_len(length(sib))) {
  
  # create covariance matrix
  # for each sibilant
  Sigma <- diag(get(sds.sib[w])) %*%
    get(cor.sib[w]) %*% 
    diag(get(sds.sib[w]))
  
  # stabilise covariance matrices
  # (avoid problems with high correlations)
  Sigma <- as.matrix(
    Matrix::nearPD(Sigma)$mat
  )
  
  # simulate the adjustments for each item
  it.adjs[
    (
      (length(t.id) / length(sib) * w) -
        length(t.id) / length(sib) + 1
    ):(
      length(t.id) / length(sib) * w
    )
    ,
  ] <- 
    MASS::mvrnorm(
      n = length(t.id) / length(sib)
      , mu = mus
      , Sigma = Sigma
    )
}

# rename the dimensions of the matrix
# with the combinations of twn and g
# to cbind with the main adjustment grid
colnames(it.adjs) <- t.g.combined

# create item grid
it.grid <- data.frame(
  item = t.id
  , sib = rep(sib, each = n.t.s)
)

# add adjustments
it.grid <- cbind(
  it.grid, it.adjs
)

# pivot to longer
# so that when joined with the observations df
# to simulate the observations
# only the corresponding item adjustments are added
# and not all
# to avoid ifelse statements in simulation
it.grid <- pivot_longer(
  it.grid
  , cols = t.g.combined
  , names_to = 't.g'
  , values_to = 'it.t.g.adj'
)

# separate combinations of twn and g
# into twn and g
it.grid <- tidyr::separate(
  it.grid
  , 't.g'
  , into = c('twn', 'g')
)

# ----simulate-df----
# simulate the observations
# with rnorm
#
# number of observations per token
n.obs.t <- 5

# create the full df
# with combinations of sp and item
# and the corresponding repetitions
df <- data.frame(
  sp = rep(
    sp.id
    , each = length(t.id) * n.obs.t
  )
  , item = rep(
    t.id
    , times = n.p * n.obs.t
  )
)

# retrieve sibilants from the item names
# to create the sib column
# makes joining much simpler
df$sib <- gsub(
  '^item|_|[0-9]'
  , ''
  , df$item
)

# join grid with sp adjustments
# brings also twn, g and age
df <- left_join(
  df
  , sp.grid
)

# join grid with item adjustments
df <- left_join(
  df
  , it.grid
)

# join grid with population=level parameters
df <- left_join(
  df
  , cat.grid
)


# transform age to centered log age
# because the population-level parameters
# were defined having this in mind
df$cl_age <- log(df$age) - mean(log(df$age))

# simulate center of gravity values
# each observations comes from a normal distribution
df$cog <- rnorm(
  nrow(df)
  , mean = df$alpha + 
    df$part.sib.adj + 
    df$it.t.g.adj + 
    df$beta * df$cl_age
  , sd = df$sigma
)

# remove unnecessary columns
df <- select(
  df
  , -contains('adj')
  , -alpha
  , -beta
  , -sigma
)

# remove everything
# but the final object
# and correlation objects
rm(
  list = setdiff(
    ls(), c(
      'df'
      , ls()[grepl(
        '^cor.|^sds.|^sigmas$|^cat.grid$|^colors|^labels'
        , ls()
      )]
      )
  )
)

# ----checks----
# 
# fit <- lme4::lmer(
#   cog ~ 0 + twn:g:sib +
#     twn:g:sib:cl_age +
#     (0 + sib | sp) +
#     (1 | item)
#   , data = df
# )
# 
# summary(fit)
# 
# data.frame(
#   emmeans::emmeans(
#     fit
#     , ~ 0 + twn:g:sib +
#       twn:g:sib:cl_age
#     , at = list(cl_age = runif(50, -.25, .25))
#   )
# ) %>%
#   ggplot(
#     aes(
#       x = exp(cl_age)
#       , y = emmean
#       , color = g
#       , group = g
#     )
#   ) +
#   geom_ribbon(
#     aes(
#       ymin = asymp.LCL
#       , ymax = asymp.UCL
#       , fill = g
#     )
#     , color = NA
#     , alpha = .2
#   ) +
#   geom_line() +
#   facet_wrap(
#     ~twn*sib
#   )
# 
# fit <- lme4::lmer(
#   scale(cog) ~ 0 + twn:g:sib +
#     twn:g:sib:cl_age +
#     (0 + sib | sp) +
#     (1 | item)
#   , data = df
# )
# 
# summary(fit)
# 
# data.frame(
#   emmeans::emmeans(
#     fit
#     , ~ 0 + twn:g:sib +
#       twn:g:sib:cl_age
#     , at = list(cl_age = runif(50, -.25, .25))
#   )
# ) %>%
#   ggplot(
#     aes(
#       x = exp(cl_age)
#       , y = emmean
#       , color = g
#       , group = g
#     )
#   ) +
#   geom_ribbon(
#     aes(
#       ymin = asymp.LCL
#       , ymax = asymp.UCL
#       , fill = g
#     )
#     , color = NA
#     , alpha = .2
#   ) +
#   geom_line() +
#   facet_wrap(
#     ~twn*sib
#   )
# 
# df <- df %>% 
#   group_by(
#     sp
#   ) %>% 
#   mutate(
#     lob_cog = (cog - mean(cog)) / sd(cog)
#   )
# 
# fit <- lme4::lmer(
#   lob_cog ~ 0 + twn:g:sib +
#     twn:g:sib:cl_age +
#     (0 + sib | sp) +
#     (1 | item)
#   , data = df
# )
# 
# summary(fit)
# 
# data.frame(
#   emmeans::emmeans(
#     fit
#     , ~ 0 + twn:g:sib +
#       twn:g:sib:cl_age
#     , at = list(cl_age = runif(50, -.25, .25))
#   )
# ) %>%
#   ggplot(
#     aes(
#       x = exp(cl_age)
#       , y = emmean
#       , color = g
#       , group = g
#     )
#   ) +
#   geom_ribbon(
#     aes(
#       ymin = asymp.LCL
#       , ymax = asymp.UCL
#       , fill = g
#     )
#     , color = NA
#     , alpha = .2
#   ) +
#   geom_line() +
#   facet_wrap(
#     ~twn*sib
#   )
