#
# aggregate_IPT.R
# N Green
#
# Group together individuals in the PREDICT-TB dataset
# to use in LTBI screening DES model


library(dplyr)

## use this to test the code ----
# 
# dummy data
# DATA_MAIN <-
#   data.frame(
#     age = c(10,20,30,40,10,20,30,40,60,70),
#     sex = c(0,1,0,1,0,1,0,1,0,1),
#     ethnicity = c(1,2,3,4,5,1,2,3,4,5),
#     countryofbirth = c("UK", "India", "Other","UK", "India",
#                        "Other", "UK", "India", "Other","UK"),
#     countryofbirthTBincidence = c(1, 2, 1, 3, 3, 4, 5, 4, 4, 5),
#     yearssinceentry = c(NA,1,2,NA,10,4,NA,2,10,NA),
#     prevbcg = c(1,1,0,1,1,0,1,1,0,1),
#     activetb_site = c(1,1,1,1,NA,0,0,0,0,NA),
#     reasonforscreening = c(1,1,2,2,3,1,1,2,2,3),
#     activetb = c(1,1,1,1,0,1,1,1,1,0),
#     qfn_plus_result = c(1,1,1,2,2,2,0,1,1,1)
#   )


# create new fields with discrete groups
DATA_MAIN$age_grp <- cut(DATA_MAIN$age, c(0,15,25,35,45,55,65,100))
DATA_MAIN$yearssinceentry_grp <- cut(DATA_MAIN$yearssinceentry, c(0,1,2,5,10,100))

## run this on the real data ----

# variables to aggregate over
fields_groups <- c(
  "age",
  "sex",
  "ethnicity",
  # "countryofbirth",
  "countryofbirthTBincidence",
  "yearssinceentry",
  "prevbcg",
  "activetb_site",
  "reasonforscreening")

# variables to obtain totals with categories
fields_out <- c(
  "activetb",
  "qfn_plus_result")


## replace DATA_MAIN with whatever the data are actually called

dat_out <- DATA_MAIN[, fields_out]
dat_groups <- DATA_MAIN[, fields_groups]


# don't know how missing data is coded
# have assumed its an NA
#
# include NAs as a seperate category
dat_groups$yearssinceentry <- factor(dat_groups$yearssinceentry, exclude = "")
dat_groups$activetb_site   <- factor(dat_groups$activetb_site, exclude = "")


# total category size
pop <- aggregate(rep(1, nrow(dat_out)),
                 by = as.list(dat_groups),
                 FUN = "sum") %>% 
  rename(pop = "x")

# active tb counts
n_tb <- aggregate(dat_out$activetb,
                  by = as.list(dat_groups),
                  FUN = function(x) sum(x == 1)) %>% 
  rename(tb = "x")

# ltbi positive counts
n_ltbi <- aggregate(dat_out$qfn_plus_result,
                    by = as.list(dat_groups),
                    FUN = function(x) sum(x == 1)) %>% 
  rename(ltbi = "x")

# combined array

res <- 
  merge(pop, n_tb,
        by = fields_groups) %>% 
  merge(n_ltbi,
        by = fields_groups)

save(res, file = "aggregated_data.RData")
