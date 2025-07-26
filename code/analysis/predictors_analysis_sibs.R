
# Load packages ----------------------------------------------------------------

library(tidyverse)
library(brms)


# Load data --------------------------------------------------------------------

edu2 <- read.csv("data/edu_preprint_data.csv")



#PREDICTORS OF EDUCATION ------------------------------------------------------
  
## Gender  ---------------------------------------------------------------------

# Do boys have a higher probability of attending school than girls?
# Are higher socio-economic status families more likely to send boys to school than girls?
# Does this change over time?

# Data: Full school records dataset


### Basic model ----------------------------------------------------------------

m.gen <- brm(edu_GenderNum ~ 0 + Intercept + 
               edu_ParentSES +
               s(edu_AdmissionYear) + 
               (1 | edu_BirthCohort)+
               (1 | match_fam_id),
             data = edu,
             family = "bernoulli",
             prior = c(
               prior(normal(0, 1), class = "b", coef = "Intercept"),
               prior(normal(0, 0.5), class = "b"),
               prior(student_t(3, 0, 2.5), class = "sd"),
               prior(student_t(3, 0, 5.9), class = "sds")),
             seed = 10,
             iter = 10000,
             cores = 4,
             control = list(adapt_delta = 0.99, max_treedepth = 12),
             file = "model_fits_new/m.gen2")







### Full model -----------------------------------------------------------------

m.gen.int <- brm(edu_GenderNum ~ 0 + Intercept + 
                   edu_ParentSES +
                   edu_AdmissionYear + 
                   edu_ParentSES:edu_AdmissionYear + 
                   (1 | edu_BirthCohort)+
                   (1 | match_fam_id),
                 data = edu,
                 family = "bernoulli",
                 prior = c(
                   prior(normal(0, 1), class = "b", coef = "Intercept"),
                   prior(normal(0, 0.5), class = "b"),
                   prior(student_t(3, 0, 2.5), class = "sd")),
                 seed = 10,
                 iter = 20000,
                 cores = 4,
                 control = list(adapt_delta = 0.95, max_treedepth = 15),
                 file = "model_fits_new/m.gen.int2")



## Leaving the islands ---------------------------------------------------------

# H3. Children from higher socio-economic status families are more likely to leave the islands

# Data: Full school records dataset
# LeavesIslands: Leaves = 1, Doesn’t leave = 0


### Basic model ----------------------------------------------------------------

m.leave.gen <- brm(edu_LeaveIslands ~ 0 + Intercept + 
                     edu_ParentSES + 
                     edu_GenderNum + 
                     s(edu_AdmissionYear) + 
                     (1 |edu_BirthCohort)+
                     (1 | match_fam_id), 
                   data = edu,
                   family = "bernoulli",
                   prior = c(
                     prior(normal(0, 1), class = "b", coef = "Intercept"),
                     prior(normal(0, 0.5), class = "b"),
                     prior(student_t(3, 0, 2.5), class = "sd"),
                     prior(student_t(3, 0, 5.9), class = "sds")),	
                   seed = 10,
                   iter = 10000,
                   cores = 4,
                   control = list(adapt_delta = 0.99),
                   file = "model_fits_new/m.leave.gen2")


### Full model -----------------------------------------------------------------

m.leave.int.gen <- brm(edu_LeaveIslands ~ 0 + Intercept + 
                         edu_ParentSES +
                         edu_GenderNum + 
                         edu_AdmissionYear +
                         edu_ParentSES:edu_GenderNum +
                         edu_ParentSES:edu_AdmissionYear +
                         (1 |edu_BirthCohort)+
                         (1 | match_fam_id),
                       data = edu,
                       family = "bernoulli",
                       prior = c(
                         prior(normal(0, 1), class = "b", coef = "Intercept"),
                         prior(normal(0, 0.5), class = "b"),
                         prior(student_t(3, 0, 2.5), class = "sd")),	
                       seed = 10,
                       iter = 20000,
                       cores = 4,
                       control = list(adapt_delta = 0.95, max_treedepth = 15),
                       file = "model_fits_new/m.leave.int.gen2")


## Reason for withdrawal -------------------------------------------------------

# H5. Children from higher socio-economic status families are more likely to leave school for education, rather than work/illness

# Data: Full school records dataset

# edu_FurtherEduVOther: 1 if left for further edu, 0 if left for other reason



### Basic model ----------------------------------------------------------------

m.reason <- brm(edu_FurtherEduVOther ~ 0 + Intercept + 
                  edu_ParentSES + 
                  edu_GenderNum + 
                  s(edu_AdmissionYear) +
                  (1 |edu_BirthCohort) +
                  (1 | match_fam_id),
                data = edu,
                family = "bernoulli",
                prior = c(
                  prior(normal(0, 1), class = "b", coef = "Intercept"),
                  prior(normal(0, 0.5), class = "b"),
                  prior(student_t(3, 0, 2.5), class = "sd"),
                  prior(student_t(3, 0, 5.9), class = "sds")),	
                seed = 10,
                iter = 20000,
                cores = 4,
                control = list(adapt_delta = 0.999),
                file = "model_fits_new/m.reason2")



### Full model -----------------------------------------------------------------

m.reason.int <- brm(edu_FurtherEduVOther ~ 0 + Intercept + 
                      edu_ParentSES + 
                      edu_GenderNum + 
                      edu_AdmissionYear +
                      edu_ParentSES:edu_GenderNum +
                      edu_ParentSES:edu_AdmissionYear +  
                      (1 |edu_BirthCohort) +
                      (1 | match_fam_id),
                    data = edu,
                    family = "bernoulli",
                    prior = c(
                      prior(normal(0, 1), class = "b", coef = "Intercept"),
                      prior(normal(0, 0.5), class = "b"),
                      prior(student_t(3, 0, 2.5), class = "sd")),	
                    seed = 10,
                    iter = 20000,
                    cores = 4,
                    control = list(adapt_delta = 0.95, max_treedepth = 15),
                    file = "model_fits_new/m.reason.int2")



## Leaving age -----------------------------------------------------------------

# H7. Children from high socio-economic status families spend longer in education

# Binomial survival model -


### Basic model ----------------------------------------------------------------

# Basic model
m.leaving.age <- brm(data = leaving.age.data.pp,
                     family = binomial,
                     event | trials(1) ~ 0 + period.f + edu_GenderNum + edu_ParentSES + s(edu_AdmissionYear) + (1|edu_BirthCohort) + (1 | match_fam_id),
                     prior = c(prior(normal(0, 1), class = "b", coef = "edu_GenderNum1"),
                               prior(normal(0, 1), class = "b", coef = "edu_ParentSES"),
                               prior(normal(0, 1), class = "b", coef = "sedu_AdmissionYear_1"),
                               prior(normal(0, 1.5), class = "b"),
                               prior(student_t(3, 0, 2.5), class = "sd"),
                               prior(student_t(3, 0, 5.9), class = "sds")),
                     chains = 4, cores = 4, iter = 20000, warmup = 1000,
                     control = list(adapt_delta = 0.999, max_treedepth = 12),
                     seed = 10,
                     file = "model_fits_new/m.leaving.age2.rds")



### Full model -----------------------------------------------------------------
m.leaving.age.int <- brm(data = leaving.age.data.pp,
                         family = binomial,
                         event | trials(1) ~ 0 + period.f + edu_GenderNum + edu_ParentSES + edu_AdmissionYear +
                           edu_ParentSES:edu_GenderNum + edu_ParentSES:edu_AdmissionYear +
                           (1|edu_BirthCohort) + (1 | match_fam_id),
                         prior = c(prior(normal(0, 1), class = "b", coef = "edu_GenderNum1"),
                                   prior(normal(0, 1), class = "b", coef = "edu_ParentSES"),
                                   prior(normal(0, 1), class = "b", coef = "edu_AdmissionYear"),
                                   prior(normal(0, 1), class = "b", coef = "edu_GenderNum1:edu_ParentSES"),
                                   prior(normal(0, 1), class = "b", coef = "edu_ParentSES:edu_AdmissionYear"),
                                   prior(normal(0, 1.5), class = "b"),
                                   prior(student_t(3, 0, 2.5), class = "sd")),
                         init = 0,
                         chains = 4, cores = 4, iter = 40000, warmup = 1000,
                         control = list(adapt_delta = 0.99, max_treedepth = 12),
                         seed = 10,
                         file = "model_fits_new/m.leaving.age2.int")



