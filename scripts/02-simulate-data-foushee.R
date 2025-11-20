source('scripts/00-setup-packages.R')

# This script simulates some data
# based in Foushee et al. 2023
# Fig. 3 in https://doi.org/10.1177/09637214221123920
# to use it to illustrate how to fit
# models with different treatments of
# categorical predictors.

# The original study had two categorical predictors
# each with 2 levels.
# I added a level to one of them,
# in order it to have three
# to illustrate how indicator and sum-coding fail
# with more than two levels

# First, a grid with the categorical
# predictors is created,
# and their corresponding alpha and beta parameters
# (the ones that should be recovered by the model)

# Second, the variation at group-level
# components are defined

# Third, participant and item
# adjustment grids are created
# Synthetic participants and items
# are created, and their
# corresponding adjustments are defined

# Fourth, the data are simulated
# using the information in the previous
# sections

# Fifth, a brief check is done,
# fitting a model
# and plotting estimates

# ----defaults----

set.seed(1)

# ----condition-grid----

# create a data frame with
# - the combinations of categorical variables,
# - their corresponding population parameters
# - their corresponding group-level sds
# used in the linear predictor

# condition kids are exposed to
cond <- c(
  'pedagogical'
  , 'overhearing'
  , 'indirect'
)

# learning conditions
task <- c('word', 'fact')

# combination grid
cond.grid <- expand.grid(cond = cond, task = task)

cond.grid$alpha <- c(
  # word learning
  0, qlogis(.45), qlogis(.4)
  # fact learning
  , qlogis(.75), qlogis(.6), qlogis(.6)
)

cond.grid$beta <- c(
  # word learning
  0.1, .15, .0
  # fact learning
  , .2, .6, .3
)

# ----group-level-adjustments----

## participant

### cond
sd.part.pedagogical <- .35
sd.part.overhearing <- .55
sd.part.indirect <- .45

sds.part.cond <- c(
  sd.part.pedagogical
  , sd.part.overhearing
  , sd.part.indirect
)

### task
sd.part.word <- .3
sd.part.fact <- .25

sds.part.task <- c(
  sd.part.word
  , sd.part.fact
)

### correlation between group-levels
rho.part <- .5

## item
sd.it.word <- .2
sd.it.fact <- .1

sds.it <- c(sd.it.word, sd.it.fact)
rho.it <- .8

# ----participant-grid----

# create a data frame with
# - participant ids
# - ages
# - conditions
# - corresponding deviations from pop avg
# used to simulate observations

# number of participants per condition
n.p <- 18

# vector of participant ids
p.id <- paste0('part_', 1:(n.p * length(cond)))

# vector of ages
age <- runif(n.p * length(cond), 3, 6)

# vector of conditions
# each participant only sees one
conds <- rep(cond, each = n.p)

# simulate individual adjustments
# from a normal distrubtion
# the sd is defined above
# in the group-level adjustments
# This could be improved,
# because right now the order of conditions
# is hard coded -> easy to make a mistake
adj.cond <- rnorm(
  n.p * length(cond)
  , mean = 0
  , sd = rep(sds.part.cond, each = n.p)
)

# create a data grid the previous vectors
part.grid <- data.frame(
  part = p.id, age, conds, adj.cond
)

# simulate by-task adjustments
# for each participant
# they come from a bivariate normal
# distribution, where the sds and correlation
# are defined above
adjustments <- extraDistr::rbvnorm(
  n = n.p * length(cond)
  , mean1 = 0
  , mean2 = 0
  , sd1 = sds.part.task[1]
  , sd2 = sds.part.task[2]
  , cor = rho.part
)

# add adjustments to the participant grid
part.grid$word <- adjustments[,1]
part.grid$fact <- adjustments[,2]

# pivot data frame to longer format
# so that task adjustments do not take
# one column each
# This makes it easier to simulate the actual
# data, because only the adjustments
# that correspond to a particular adjustment
# will be present
# The alternative would be an ifelse statement
# in the simulation step
part.grid <- pivot_longer(
  part.grid
  , cols = c('word', 'fact')
  , names_to = 'task'
  , values_to = 'part.cond.adj'
)

# ----token-grid----

# This section simulates a grid of
# by-item adjustments
# For simplicity,
# there is no by-condition adjustments

# number of tokens per kid and condition
n.t <- 9

# vector of item ids
t.id <- paste0('item_', 1:n.t)

# simulate adjustments
# from bivariate normal
adjustments <- extraDistr::rbvnorm(
  n = n.t
  , mean1 = 0
  , mean2 = 0
  , sd1 = sds.it[1]
  , sd2 = sds.it[2]
  , cor = rho.it
)

# create grid of adjustments
it.grid <- data.frame(
  item = t.id
  , word = adjustments[,1]
  , fact = adjustments[,2]
)

# pivot to longer,
# so that the joining below
# only picks the ajdustments
# that correspond to that particular observation
# and avoid ifelse statements in simulation
it.grid <- pivot_longer(
  it.grid
  , cols = c('word', 'fact')
  , names_to = 'task'
  , values_to = 'it.cond.adj'
)

# ----simulate-df----

# This section simulates the actual data
# First, a grid is created
# with the corresponding combinations
# of the variables at play

# Then the adjustments are left joined
# from the objects created before

# Finally, the actual data are simulated

# create the actual data frame
# each line has to correspond to one observation

df <- data.frame(
  # participant
  part = rep(
    p.id
    # each participant gives
    # one answer per item per task
    , each = length(task) * n.t
  )
  # age
  , age = rep(
    # ages correspond to participants
    # so like participant
    age
    , each = length(task) * n.t
  )
  # condition
  , cond = rep(
    cond
    # each condition is repeated by
    # participant (n.p)
    # times the answers by participant
    , each = length(task) * n.t * n.p
  )
  # items
  , item = rep(
    t.id
    # items are repeated by participant
    # times the number of task-conditions
    , times = length(task) * n.p
  )
  # task
  # each task is repeated the number of items
  # and then times the number of participants
  , task = rep(
    rep(
      task
      , each = n.t
    )
    , times = n.p
  )
)

# left join the grids
# with the parameter values
# to use in the simulation

# population-level adjustments
df <- left_join(
  df
  , cond.grid
)

# by-participant adjustments
df <- left_join(
  df
  , part.grid
)

# by-item adjustments
df <- left_join(
  df
  , it.grid
)

# create centered age
# because the intercepts
# were designed at ~4.5 years of age
# following Foushee et al.
df$c_age <- df$age - mean(df$age)

# simulate response
# from a binomial distribution
# the prob value is the linear predictor
# in the log-odds space
# transformed into proportions
df$acc <- rbinom(
  nrow(df)
  , 1
  , plogis(
    df$alpha + 
      df$adj.cond + 
      df$part.cond.adj + 
      df$it.cond.adj +
      df$beta * df$c_age
  )
)

# remove the variables used for simulation
df <- select(
  df
  , -contains('adj')
  , -alpha
  , -beta
)


# ----conditions-tbl----

cond.tbl <- cond.grid %>% 
  select(cond, task) %>% 
  mutate_if(
    is.factor
    , as.character
  ) %>% 
  mutate_if(
    is.character
    , snakecase::to_sentence_case
  ) %>%
  arrange(cond) %>% 
  mutate(comb = paste(
    substr(cond, 1, 1)
    , substr(task, 1, 1)
    , sep = "&")) %>% 
  pivot_wider(
    , names_from = task
    , values_from = comb
  ) 



# ----remove-garbage----
# remove garbage
# all but the final df
rm(list = setdiff(ls(), c('df', 'cond.tbl')))

# ----checks----
# 
# fit <- glmer(
#   acc ~ 0 + cond:task +
#     cond:task:c_age +
#     (0 + task | part) +
#     (1 | item)
#   , data = df
#   , family = 'binomial'
# )
# 
# summary(fit)
# 
# data.frame(
#   emmeans(
#     fit
#     , ~ 0 + cond:task +
#             cond:task:c_age
#     , at = list(c_age = runif(50, -2.5, 2.5))
#     , type = 'response')
# ) %>% 
#   ggplot(
#     aes(
#       x = c_age
#       , y = prob
#       , color = cond
#       , group = cond
#     )
#   ) +
#   geom_ribbon(
#     aes(
#       ymin = asymp.LCL
#       , ymax = asymp.UCL
#       , fill = cond
#     )
#     , color = NA
#     , alpha = .2
#   ) +
#   geom_line() +
#   facet_wrap(
#     ~task
#   )
