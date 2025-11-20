source('scripts/00-setup-packages.R')

# ----read-salig2024----

salig.df <- read.csv(
  'data/salig2024.csv'
)

# variable renaming
# to type less

# subject id to S
colnames(salig.df) <- 
  gsub(
    '^id$'
    , 'S'
    , colnames(salig.df)
  )

# group to G
# otherwise marginaleffects does tot work
colnames(salig.df) <- 
  gsub(
    '^group$'
    , 'G'
    , colnames(salig.df)
  )

# reaction times to T
colnames(salig.df) <- 
  gsub(
    '^QuestionRT$'
    , 'T'
    , colnames(salig.df)
  )

# baselang to L
colnames(salig.df) <- 
  gsub(
    '^baselang$'
    , 'L'
    , colnames(salig.df)
  )

# item number to I
colnames(salig.df) <- 
  gsub(
    '^itemnum$'
    , 'I'
    , colnames(salig.df)
  )

