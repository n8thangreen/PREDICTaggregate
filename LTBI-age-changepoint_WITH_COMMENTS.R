
## change point analysis of LTBI prevalence by age
## N Green (25/09/2019)


library(purrr)
library(bcp)
library(dplyr)


# LTBI by age RCS ---


## group sample sizes
## ------------------
## the number of counts in each age group need to be comparable
## so if scale them to a common hypothetical cohort size e.g. 1 (proportion)
## then we loose the variability due to sample size
## if the group sizes are roungly the same then I'm not too worried about this
## but if they are very different they we could just write the sampler code ourselves
## to adjust for this, rather than using the package functions


## read in the data

## I've assumed its in the form of a dataframe 'dat'
## with columns 'age' and 'qfngit_results'

## generate some dummy data

n <- 1000    # cohort sample size
ppos1 <- 0.6 # probability of positive test
ppos2 <- 0.4

dat <-
  data.frame(qfngit_results =                                     # selected QFN-GIT from available tests
               c(rbinom(n = n/2, size = 1, prob = ppos1),         # positive or negative test results
                 rbinom(n = n/2, size = 1, prob = ppos2)),        # with change point at half the age range
             age = rep(20:39, each = n/20),                       # sequential ages with repeats
             yearssinceentry =
               pmax(0,rep(20:39, each = n/20) - rexp(n = n, rate = 0.4))) # arbitrary function of age to test correlation

## stratify by years since entry
# dat <- dat[dat$yearssinceentry < 5, ]
# dat <- dat[dat$yearssinceentry >= 5, ]

## aggregate over ages
dat2 <- 
  dat %>% 
  group_by(age) %>% 
  summarise(
    mean_results = mean(qfngit_results),
    total_results = sum(qfngit_results),
    mean_entryuk = mean(yearssinceentry))

# fit model
bcp1 <- bcp(dat2$mean_results)
# bcp1 <- bcp(dat2$total_results)

## posterior means and change point distributions
plot(bcp1, main = "LTBI by age RCS")



# LTBI by age RCS (migrants, adj for time since arrival) ---

## read in the data
## assign the LTBI counts or proportion to y

# fit model
bcp2 <- bcp(dat$qfngit_results)
plot(bcp2, main = "LTBI by age RCS - migrants, adj for time since arrival")



# other packages  -------------------------------------------------------------

library(EnvCpt)
out <- envcpt(dat2$mean_results)
AIC(out)
plot(out)

library(changepoint)
out <- cpt.mean(dat2$mean_results)
plot(out)

