#
# aggregate_IPD.R
# N Green
# Imperial College London
#
# Group together individuals in the PREDICT-TB dataset
# to use in LTBI screening DES model


library(dplyr)



# use this to test the code ----
 
# dummy data
DATA_MAIN <-
  data.frame(
    age = c(10,20,30,40,10,20,30,40,60,70),
    sex = c(0,1,0,1,0,1,0,1,0,1),
    
    ##TODO:
    # what is this ethnicity variable name in the original data?
    # what are the numbers for each group?
    ethnicity = c(1,2,3,4,5,1,2,3,4,5),
    ethnicity0 = c("Pakistan", "Indian", "White", "White", "Indian",
                   "Mixed/Other", "White", "Bangladeshi", "Black Other", "Black African"),
    
    countryofbirth = c("UK", "India", "Other","UK", "India",
                       "Other", "UK", "India", "Other","UK"),
    borninstudycountry = c(1,0,0,1,0,0,1,0,0,1),
    countryofbirthTBincidence = c(1, 2, 1, 3, 3, 4, 5, 4, 4, 5),
    yearssinceentry = c(NA,1,2,NA,10,4,NA,2,10,NA),
    prevbcg = c(1,1,0,1,1,0,1,1,0,1),
    activetb_site = c(1,1,1,1,NA,0,0,0,0,NA),
    reasonforscreening = c(1,1,2,2,3,1,1,2,2,3),
    activetb = c(1,1,1,1,0,1,1,1,1,0),
    qfn_plus_result = c(1,1,1,2,2,2,0,1,1,1),
    pos_either = c(1,1,1,2,2,2,0,1,1,1),
    qfngit_result = c(1,1,1,1,0,0,1,0,0,0),
    qfnplus_result = c(1,1,1,1,1,1,0,0,0,1),
    tspot_result = c(1,0,1,1,1,1,1,1,0,0),
    mantoux_result = c(2,3,4,5,6,10,17,11,12,15)
  )


# Read in data ----
# 
# setwd("~/Rishi Gupta/Peter White NIHR HTA")
# DATA_MAIN <- read.csv("Abubakar2018.csv")
# DATA_MAIN <- DATA_MAIN %>% mutate_all(na_if, "")
# 
# # Change composite IGRA variable (pos_either) to numeric
# 
# DATA_MAIN$pos_either <- as.character(DATA_MAIN$pos_either)
# DATA_MAIN$pos_either[DATA_MAIN$pos_either == "Positive"] <- 1
# DATA_MAIN$pos_either[DATA_MAIN$pos_either == "Negative"] <- 0
# DATA_MAIN$pos_either <- as.numeric(DATA_MAIN$pos_either)
# 
# 
# ###########################
# ## Drop patients with missing IGRA
# ## (otherwise we were getting NAs in the LTBI column in the final res dataframe)
# 
# # Q: is this correct? why is it a 3D array? (NG)
# comp_IGRA <- complete.cases(DATA_MAIN[ , "pos_either", ])
# DATA_MAIN <- DATA_MAIN[comp_IGRA, ]
# 
# # Q: would this be simpler? (NG)
# DATA_MAIN <- DATA_MAIN[!is.na(DATA_MAIN$pos_either), ]
# 
# ############################


# create new fields with discrete groups
DATA_MAIN$age_grp <- cut(DATA_MAIN$age,
                         breaks = c(0,15,35,55,100))

DATA_MAIN$yearssinceentry_grp <- cut(DATA_MAIN$yearssinceentry,
                                     breaks = c(0,2,5,100))

DATA_MAIN$mantoux_5mm  <- as.numeric(DATA_MAIN$mantoux_result > 5)
DATA_MAIN$mantoux_10mm <- as.numeric(DATA_MAIN$mantoux_result > 10)
DATA_MAIN$mantoux_15mm <- as.numeric(DATA_MAIN$mantoux_result > 15)

## run this on the real data ----

# variables to aggregate over
fields_groups <-
  c(
    "age_grp",
    # "ethnicity",
    "ethnicity0",
    "borninstudycountry",
    "yearssinceentry_grp",
    "prevbcg",
    "reasonforscreening"
    # "sex",
    # "countryofbirth",
    # "inc_cob_participant2",
    #"activetb_site",   # This is the site of disease in the study participant
                        # (which is an outcome rather than exposure - I have therefore removed it)
  )

# variables to obtain totals with categories
fields_out <-
  c(
    "activetb",
    # "pos_either"
    "qfngit_result", 
    "qfnplus_result",
    "tspot_result",
    "mantoux_5mm", 
    "mantoux_10mm",
    "mantoux_15mm"
  )


## replace DATA_MAIN with whatever the data are actually called

dat_out <- DATA_MAIN[, fields_out]
dat_groups <- DATA_MAIN[, fields_groups]


##TODO:
# don't know how missing data is coded
# have assumed its an NA
#
# include NAs as a seperate category
# (I have added some more variables here where there is a significant amount of missing data)

# dat_groups$activetb_site <- factor(dat_groups$activetb_site, exclude = "")
# dat_groups$inc_cob_participant2 <- factor(dat_groups$inc_cob_participant2, exclude = "")
dat_groups$yearssinceentry_grp <- factor(dat_groups$yearssinceentry_grp, exclude = "")
dat_groups$prevbcg <- factor(dat_groups$prevbcg, exclude = "")

# total category size
pop <-
  aggregate(rep(1, nrow(dat_out)),
            by = as.list(dat_groups),
            FUN = "sum") %>% 
  rename(pop = "x")

# active tb counts
n_tb <-
  aggregate(dat_out$activetb,
            by = as.list(dat_groups),
            FUN = function(x) sum(x == 1)) %>% 
  rename(tb = "x")

# ltbi test positive counts
n_ltbi <-
  aggregate(dat_out$pos_either,
            by = as.list(dat_groups),
            FUN = function(x) sum(x == 1)) %>% 
  rename(ltbi = "x")

n_qfngit <-
  aggregate(dat_out$qfngit_result,
            by = as.list(dat_groups),
            FUN = function(x) sum(x == 1)) %>% 
  rename(qftgit = "x")

n_qfnplus <-
    aggregate(dat_out$qfnplus_result,
            by = as.list(dat_groups),
            FUN = function(x) sum(x == 1)) %>% 
  rename(qfnplus = "x")

n_tspot <-
  aggregate(dat_out$tspot_result,
            by = as.list(dat_groups),
            FUN = function(x) sum(x == 1)) %>% 
  rename(tspot = "x")

n_mantoux_5mm <-
  aggregate(dat_out$mantoux_5mm,
            by = as.list(dat_groups),
            FUN = function(x) sum(x == 1)) %>% 
  rename(mantoux_5mm = "x")

n_mantoux_10mm <-
  aggregate(dat_out$mantoux_10mm,
            by = as.list(dat_groups),
            FUN = function(x) sum(x == 1)) %>% 
  rename(mantoux_10mm = "x")

n_mantoux_15mm <-
  aggregate(dat_out$mantoux_15mm,
            by = as.list(dat_groups),
            FUN = function(x) sum(x == 1)) %>% 
  rename(mantoux_15mm = "x")

# combined array

res <- 
  merge(pop, n_tb,
        by = fields_groups) %>% 
  # merge(n_ltbi,
  #       by = fields_groups) %>% 
  merge(n_qfngit,
        by = fields_groups) %>% 
  merge(n_qfnplus,
        by = fields_groups) %>% 
  merge(n_tspot,
        by = fields_groups) %>% 
  merge(n_mantoux_5mm,
        by = fields_groups) %>% 
  merge(n_mantoux_10mm,
        by = fields_groups) %>% 
  merge(n_mantoux_15mm,
        by = fields_groups)

  
########
# save #
########

save(res, file = "aggregated_data.RData")
write.csv(res, "aggregated_data.csv")
