
library(tidyverse)
library(brms)
library(patchwork)
library(viridis)
library(ggplot2)
library(tidybayes)
library(bayesplot)
library(ggdist)
library(parallel)
library(hexbin)
library(shinystan)
library(bayestestR)



# Load data -------

edu2 <- read.csv("data/edu_preprint_data.csv")



# OUTCOMES OF EDUCATION --------------------------------------------------------


## Adult SES -------------------------------------------------------------------

# H9. Children who spend longer in education have higher adult SES 

# Data: those who stayed on the islands


### Basic model ----------------------------------------------------------------

m.ses <- brm(adult_SES ~ 0 + Intercept + 
               edu_SchoolLeavingAge + 
               s(edu_AdmissionYear) +
               (1 | edu_BirthCohort) +
               (1 | match_fam_id),
             data = subset(edu2, edu_LeaveIslands == 0),
             prior = c(
               prior(normal(50, 30), class = "b", coef = "Intercept"),
               prior(normal(0, 10), class = "b"),
               prior(student_t(3, 0, 7.6), class = "sd"),
               prior(student_t(3, 0, 5.9), class = "sds")),
             seed = 10,
             iter = 6000,
             control = list(adapt_delta = 0.9999, max_treedepth = 15),
             cores = parallel::detectCores(),
             file = "model_fits_new/m.ses2")



### Full model  -----------------------------------------------------------------

m.ses.int <- brm(adult_SES ~ 0 + Intercept + 
                   edu_SchoolLeavingAge + 
                   edu_AdmissionYear +
                   edu_SchoolLeavingAge:edu_AdmissionYear +
                   (1 | edu_BirthCohort)+
                   (1 | match_fam_id),
                 data = subset(edu2, edu_LeaveIslands == 0),
                 prior = c(
                   prior(normal(50, 30), class = "b", coef = "Intercept"),
                   prior(normal(0, 10), class = "b"),
                   prior(student_t(3, 0, 7.6), class = "sd")),
                 seed = 10,
                 iter = 10000,
                 control = list(adapt_delta = 0.95, max_treedepth = 15),
                 cores = parallel::detectCores(),
                 file = "model_fits_new/m.ses.int2")



## Adult family size -----------------------------------------------------------

# H10. Children who spent longer in education have smaller adult family size 

# Data: those who stayed on the islands


### Basic model ----------------------------------------------------------------

m.fert <- brm(adult_Fertility ~ 0 + Intercept + 
                edu_SchoolLeavingAge + 
                s(edu_AdmissionYear)  +
                (1 | edu_BirthCohort) + 
                (1 | match_fam_id),
              data = subset(edu2, edu_LeaveIslands == 0),
              family = "negbinomial",
              prior = c(
                prior(normal(0, 0.2), class = "b"),
                prior(student_t(3, 0, 2.5), class = "sd"),
                prior(student_t(3, 0, 5.9), class = "sds"),
                prior(gamma(0.01, 0.01), class = shape)),
              seed = 10,
              iter = 4000,
              control = list(adapt_delta = 0.9999),
              cores = 4,
              file = "model_fits_new/m.fert2")



### Full model -----------------------------------------------------------------

m.fert.int <- brm(adult_Fertility ~ 0 + Intercept + 
                    edu_SchoolLeavingAge + 
                    edu_AdmissionYear +
                    edu_SchoolLeavingAge:edu_AdmissionYear +
                    (1 | edu_BirthCohort)+ 
                    (1 | match_fam_id),
                  data = subset(edu2, edu_LeaveIslands == 0),
                  family = "negbinomial",
                  prior = c(
                    prior(normal(0, 0.2), class = "b"),
                    prior(student_t(3, 0, 2.5), class = "sd"),
                    prior(gamma(0.01, 0.01), class = shape)),
                  seed = 10,
                  iter = 10000,
                  control = list(adapt_delta = 0.99),
                  cores = 4,
                  file = "model_fits_new/m.fert.int2")



## Lifespan --------------------------------------------------------------------

# H10. Children who spent longer in education have longer lifespans

# Data: those who stayed on the islands

# Binomial survival model (grouped by decade) 


### Basic model ----------------------------------------------------------------

m.lifespan <- brm(data = lifespan.data3.pp,
                  family = binomial,
                  event | trials(1) ~ 0 + period.f  + edu_SchoolLeavingAge + s(edu_AdmissionYear) + (1|edu_BirthCohort) + (1 | match_fam_id),
                  prior = c(prior(normal(0, 1), class = "b", coef = "edu_SchoolLeavingAge"),
                            prior(normal(0, 1), class = "b", coef = "sedu_AdmissionYear_1"),
                            prior(normal(0, 1.5), class = "b"),
                            prior(student_t(3, 0, 2.5), class = "sd"),
                            prior(student_t(3, 0, 5.9), class = "sds")),
                  cores = 4, iter = 4000, warmup = 1000,
                  control = list(adapt_delta = 0.99),
                  seed = 10,
                  file = "model_fits_new/m.lifespan2")




### Full model -----------------------------------------------------------------
m.lifespan.int <- brm(data = lifespan.data3.pp,
                      family = binomial,
                      event | trials(1) ~ 0 + period.f  + edu_SchoolLeavingAge + edu_AdmissionYear + edu_AdmissionYear:edu_SchoolLeavingAge + (1|edu_BirthCohort) + (1 | match_fam_id),
                      prior = c(prior(normal(0, 1), class = "b", coef = "edu_SchoolLeavingAge"),
                                prior(normal(0, 1), class = "b", coef = "edu_AdmissionYear"),
                                prior(normal(0, 1), class = "b", coef = "edu_SchoolLeavingAge:edu_AdmissionYear"),
                                prior(normal(0, 1.5), class = "b"),
                                prior(student_t(3, 0, 2.5), class = "sd")),
                      cores = 4, iter = 100000, warmup = 1000,
                      control = list(max_treedepth = 15),
                      init = 0,
                      seed = 10,
                      file = "model_fits_new/m.lifespan.int2")

