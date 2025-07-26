# Load packages ----------------------------------------------------------------

library(tidyverse)
library(brms)


# Load data --------------------------------------------------------------------

## Full data -------------------------------------------------------------------

edu2 <- read.csv("data/edu_preprint_data.csv")



# PREDICTORS OF EDUCATION ------------------------------------------------------

## Gender  ---------------------------------------------------------------------

#### Weaker prior  --------------------------------------------------------------

m.gen.weak <- brm(edu_GenderNum ~ 0 + Intercept + 
                    edu_ParentSES +
                    s(edu_AdmissionYear) + 
                    (1 | edu_BirthCohort) +
                    (1 | match_fam_id),
                  data = edu,
                  family = "bernoulli",
                  prior = c(
                    prior(normal(0, 1), class = "b", coef = "Intercept"),
                    prior(normal(0, 1.5), class = "b"),
                    prior(student_t(3, 0, 2.5), class = "sd"),
                    prior(student_t(3, 0, 5.9), class = "sds")),
                  seed = 10,
                  iter = 10000,
                  cores = 4,
                  control = list(adapt_delta = 0.99, max_treedepth = 12),
                  file = "model_fits/sens_new/m.gen.weak2")




#### Stronger prior  ------------------------------------------------------------

m.gen.strong <- brm(edu_GenderNum ~ 0 + Intercept + 
                      edu_ParentSES +
                      s(edu_AdmissionYear) + 
                      (1 | edu_BirthCohort) +
                      (1 | match_fam_id),
                    data = edu,
                    family = "bernoulli",
                    prior = c(
                      prior(normal(0, 1), class = "b", coef = "Intercept"),
                      prior(normal(0, 0.15), class = "b"),
                      prior(student_t(3, 0, 2.5), class = "sd"),
                      prior(student_t(3, 0, 5.9), class = "sds")),
                    seed = 10,
                    iter = 10000,
                    cores = 4,
                    control = list(adapt_delta = 0.99, max_treedepth = 12),
                    file = "model_fits/sens_new/m.gen.strong2")




### Full model -----------------------------------------------------------------
#### Weaker prior  --------------------------------------------------------------

m.gen.int.weak <- brm(edu_GenderNum ~ 0 + Intercept + 
                        edu_ParentSES +
                        edu_AdmissionYear + 
                        edu_ParentSES:edu_AdmissionYear + 
                        (1 | edu_BirthCohort) +
                        (1 | match_fam_id),
                      data = edu,
                      family = "bernoulli",
                      prior = c(
                        prior(normal(0, 1), class = "b", coef = "Intercept"),
                        prior(normal(0, 1.5), class = "b"),
                        prior(student_t(3, 0, 2.5), class = "sd")),
                      seed = 10,
                      iter = 20000,
                      cores = 4,
                      control = list(adapt_delta = 0.99, max_treedepth = 15),
                      file = "model_fits/sens_new/m.gen.int.weak2")


#### Stronger prior  ------------------------------------------------------------

m.gen.int.strong <- brm(edu_GenderNum ~ 0 + Intercept + 
                          edu_ParentSES +
                          edu_AdmissionYear + 
                          edu_ParentSES:edu_AdmissionYear + 
                          (1 | edu_BirthCohort) +
                          (1 | match_fam_id),
                        data = edu,
                        family = "bernoulli",
                        prior = c(
                          prior(normal(0, 1), class = "b", coef = "Intercept"),
                          prior(normal(0, 0.15), class = "b"),
                          prior(student_t(3, 0, 2.5), class = "sd")),
                        seed = 10,
                        iter = 20000,
                        cores = 4,
                        control = list(adapt_delta = 0.95, max_treedepth = 15),
                        file = "model_fits/sens_new/m.gen.int.strong2")



## Leaving the islands ---------------------------------------------------------

### Basic model ----------------------------------------------------------------
#### Weaker prior --------------------------------------------------------------

m.leave.weak.gen <- brm(edu_LeaveIslands ~ 0 + Intercept + 
                          edu_ParentSES + 
                          edu_GenderNum + 
                          s(edu_AdmissionYear) + 
                          (1 |edu_BirthCohort) +
                          (1 | match_fam_id), 
                        data = edu,
                        family = "bernoulli",
                        prior = c(
                          prior(normal(0, 1), class = "b", coef = "Intercept"),
                          prior(normal(0, 1.5), class = "b"),
                          prior(student_t(3, 0, 2.5), class = "sd"),
                          prior(student_t(3, 0, 5.9), class = "sds")),	
                        seed = 10,
                        iter = 10000,
                        cores = 4,
                        control = list(adapt_delta = 0.99),
                        file = "model_fits/sens_new/m.leave.weak.gen2")

#### Stronger prior  ------------------------------------------------------------

m.leave.strong.gen <- brm(edu_LeaveIslands ~ 0 + Intercept + 
                            edu_ParentSES + 
                            edu_GenderNum + 
                            s(edu_AdmissionYear) + 
                            (1 |edu_BirthCohort) +
                            (1 | match_fam_id), 
                          data = edu,
                          family = "bernoulli",
                          prior = c(
                            prior(normal(0, 1), class = "b", coef = "Intercept"),
                            prior(normal(0, 0.15), class = "b"),
                            prior(student_t(3, 0, 2.5), class = "sd"),
                            prior(student_t(3, 0, 5.9), class = "sds")),	
                          seed = 10,
                          iter = 10000,
                          cores = 4,
                          control = list(adapt_delta = 0.99),
                          file = "model_fits/sens_new/m.leave.strong.gen2")



### Full model -----------------------------------------------------------------
#### Weaker prior --------------------------------------------------------------

m.leave.int.weak.gen <- brm(edu_LeaveIslands ~ 0 + Intercept + 
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
                              prior(normal(0, 1.5), class = "b"),
                              prior(student_t(3, 0, 2.5), class = "sd")),	
                            seed = 10,
                            iter = 20000,
                            cores = 4,
                            control = list(adapt_delta = 0.95, max_treedepth = 15),
                            file = "model_fits/sens_new/m.leave.int.weak.gen2")


#### Stronger prior  ------------------------------------------------------------

m.leave.int.strong.gen <- brm(edu_LeaveIslands ~ 0 + Intercept + 
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
                                prior(normal(0, 0.15), class = "b"),
                                prior(student_t(3, 0, 2.5), class = "sd")),	
                              seed = 10,
                              iter = 20000,
                              cores = 4,
                              control = list(adapt_delta = 0.95, max_treedepth = 15),
                              file = "model_fits/sens_new/m.leave.int.strong.gen2")



## Reason for withdrawal  -------------------------------------------------------

### Basic model ----------------------------------------------------------------
#### Weaker prior  --------------------------------------------------------------

m.reason.weak <- brm(edu_FurtherEduVOther ~ 0 + Intercept + 
                       edu_ParentSES + 
                       edu_GenderNum + 
                       s(edu_AdmissionYear) +
                       (1 |edu_BirthCohort) +
                       (1 | match_fam_id),
                     data = edu,
                     family = "bernoulli",
                     prior = c(
                       prior(normal(0, 1), class = "b", coef = "Intercept"),
                       prior(normal(0, 1.5), class = "b"),
                       prior(student_t(3, 0, 2.5), class = "sd"),
                       prior(student_t(3, 0, 5.9), class = "sds")),	
                     seed = 10,
                     iter = 20000,
                     cores = 4,
                     control = list(adapt_delta = 0.999),
                     file = "model_fits/sens_new/m.reason.weak2")


#### Stronger prior  ------------------------------------------------------------

m.reason.strong <- brm(edu_FurtherEduVOther ~ 0 + Intercept + 
                         edu_ParentSES + 
                         edu_GenderNum + 
                         s(edu_AdmissionYear) +
                         (1 |edu_BirthCohort) +
                         (1 | match_fam_id),
                       data = edu,
                       family = "bernoulli",
                       prior = c(
                         prior(normal(0, 1), class = "b", coef = "Intercept"),
                         prior(normal(0, 0.15), class = "b"),
                         prior(student_t(3, 0, 2.5), class = "sd"),
                         prior(student_t(3, 0, 5.9), class = "sds")),	
                       seed = 10,
                       iter = 20000,
                       cores = 4,
                       control = list(adapt_delta = 0.999),
                       file = "model_fits/sens_new/m.reason.strong2")



### Full model -----------------------------------------------------------------
#### Weaker prior  --------------------------------------------------------------

m.reason.int.weak <- brm(edu_FurtherEduVOther ~ 0 + Intercept + 
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
                           prior(normal(0, 1.5), class = "b"),
                           prior(student_t(3, 0, 2.5), class = "sd")),	
                         seed = 10,
                         iter = 20000,
                         cores = 4,
                         control = list(adapt_delta = 0.99, max_treedepth = 15),
                         file = "model_fits/sens_new/m.reason.int.weak2")


#### Stronger prior  ------------------------------------------------------------

m.reason.int.strong <- brm(edu_FurtherEduVOther ~ 0 + Intercept + 
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
                             prior(normal(0, 0.15), class = "b"),
                             prior(student_t(3, 0, 2.5), class = "sd")),	
                           seed = 10,
                           iter = 20000,
                           cores = 4,
                           control = list(adapt_delta = 0.95, max_treedepth = 15),
                           file = "model_fits/sens_new/m.reason.int.strong2")




## Leaving age  -----------------------------------------------------------------

### Basic model ----------------------------------------------------------------
#### Weaker prior  --------------------------------------------------------------

# Basic model
m.leaving.age.weak <- brm(data = leaving.age.data.pp,
                          family = binomial,
                          event | trials(1) ~ 0 + period.f + edu_GenderNum + edu_ParentSES + s(edu_AdmissionYear) + (1|edu_BirthCohort) +
                            (1 | match_fam_id),
                          prior = c(prior(normal(0, 1.5), class = "b", coef = "edu_GenderNum1"),
                                    prior(normal(0, 1.5), class = "b", coef = "edu_ParentSES"),
                                    prior(normal(0, 1.5), class = "b", coef = "sedu_AdmissionYear_1"),
                                    prior(normal(0, 1.5), class = "b"),
                                    prior(student_t(3, 0, 2.5), class = "sd"),
                                    prior(student_t(3, 0, 5.9), class = "sds")),
                          chains = 4, cores = 4, iter = 20000, warmup = 1000,
                          control = list(adapt_delta = 0.999, max_treedepth = 12),
                          seed = 10,
                          file = "model_fits/sens_new/m.leaving.age.weak2.rds")


#### Stronger prior  ------------------------------------------------------------

# Basic model
m.leaving.age.strong <- brm(data = leaving.age.data.pp,
                            family = binomial,
                            event | trials(1) ~ 0 + period.f + edu_GenderNum + edu_ParentSES + s(edu_AdmissionYear) + (1|edu_BirthCohort) +
                              (1 | match_fam_id),
                            prior = c(prior(normal(0, 0.15), class = "b", coef = "edu_GenderNum1"),
                                      prior(normal(0, 0.15), class = "b", coef = "edu_ParentSES"),
                                      prior(normal(0, 0.15), class = "b", coef = "sedu_AdmissionYear_1"),
                                      prior(normal(0, 1.5), class = "b"),
                                      prior(student_t(3, 0, 2.5), class = "sd"),
                                      prior(student_t(3, 0, 5.9), class = "sds")),
                            chains = 4, cores = 4, iter = 20000, warmup = 1000,
                            control = list(adapt_delta = 0.999, max_treedepth = 12),
                            seed = 10,
                            file = "model_fits/sens_new/m.leaving.age.strong2.rds")



# OUTCOMES OF EDUCATION --------------------------------------------------------



## Adult SES  -----------------------------------------------------------------


### Basic model ----------------------------------------------------------------
#### Weaker prior --------------------------------------------------------------

m.ses.weak <- brm(adult_SES ~ 0 + Intercept + 
                    edu_SchoolLeavingAge + 
                    s(edu_AdmissionYear) +
                    (1 | edu_BirthCohort) +
                    (1 | match_fam_id),
                  data = subset(edu, edu_LeaveIslands == 0),
                  prior = c(
                    prior(normal(50, 30), class = "b", coef = "Intercept"), # HISCAM is standardised to this dist.
                    prior(normal(0, 20), class = "b"), # 0.10
                    prior(student_t(3, 0, 7.6), class = "sd"),
                    prior(student_t(3, 0, 5.9), class = "sds")),
                  seed = 10,
                  iter = 6000,
                  control = list(adapt_delta = 0.999),
                  cores = parallel::detectCores(),
                  file = "model_fits/sens_new/m.ses.weak2")



#### Stronger prior ------------------------------------------------------------

m.ses.strong <- brm(adult_SES ~ 0 + Intercept + 
                      edu_SchoolLeavingAge + 
                      s(edu_AdmissionYear) +
                      (1 | edu_BirthCohort) +
                      (1 | match_fam_id),
                    data = subset(edu, edu_LeaveIslands == 0),
                    prior = c(
                      prior(normal(50, 30), class = "b", coef = "Intercept"), # HISCAM is standardised to this dist.
                      prior(normal(0, 5), class = "b"), # 0.10
                      prior(student_t(3, 0, 7.6), class = "sd"),
                      prior(student_t(3, 0, 5.9), class = "sds")),
                    seed = 10,
                    iter = 6000,
                    control = list(adapt_delta = 0.999),
                    cores = parallel::detectCores(),
                    file = "model_fits/sens_new/m.ses.strong2")


### Full model -----------------------------------------------------------------
#### Weaker prior  --------------------------------------------------------------

m.ses.int.weak <- brm(adult_SES ~ 0 + Intercept + 
                        edu_SchoolLeavingAge + 
                        edu_AdmissionYear +
                        edu_SchoolLeavingAge:edu_AdmissionYear +
                        (1 | edu_BirthCohort) +
                        (1 | match_fam_id),
                      data = subset(edu, edu_LeaveIslands == 0),
                      prior = c(
                        prior(normal(50, 30), class = "b", coef = "Intercept"), # HISCAM is standardised to this dist.
                        prior(normal(0, 20), class = "b"),
                        prior(student_t(3, 0, 7.6), class = "sd")),
                      seed = 10,
                      iter = 10000,
                      control = list(adapt_delta = 0.99, max_treedepth = 15),
                      cores = parallel::detectCores(),
                      file = "model_fits/sens_new/m.ses.int.weak2")



#### Stronger prior  ------------------------------------------------------------

m.ses.int.strong <- brm(adult_SES ~ 0 + Intercept + 
                          edu_SchoolLeavingAge + 
                          edu_AdmissionYear +
                          edu_SchoolLeavingAge:edu_AdmissionYear +
                          (1 | edu_BirthCohort) +
                          (1 | match_fam_id),
                        data = subset(edu, edu_LeaveIslands == 0),
                        prior = c(
                          prior(normal(50, 30), class = "b", coef = "Intercept"), # HISCAM is standardised to this dist.
                          prior(normal(0, 5), class = "b"),
                          prior(student_t(3, 0, 7.6), class = "sd")),
                        seed = 10,
                        iter = 10000,
                        control = list(adapt_delta = 0.95, max_treedepth = 15),
                        cores = parallel::detectCores(),
                        file = "model_fits/sens_new/m.ses.int.strong2")




## Adult family size  -----------------------------------------------------------


### Basic model ----------------------------------------------------------------
#### Weaker prior  --------------------------------------------------------------

m.fert.weak <- brm(adult_Fertility ~ 0 + Intercept + 
                     edu_SchoolLeavingAge + 
                     s(edu_AdmissionYear)  +
                     (1 | edu_BirthCohort) +
                     (1 | match_fam_id),
                   data = subset(edu, edu_LeaveIslands == 0),
                   family = "negbinomial",
                   prior = c(
                     prior(normal(0, 0.6), class = "b"),
                     prior(student_t(3, 0, 2.5), class = "sd"),
                     prior(student_t(3, 0, 5.9), class = "sds"),
                     prior(gamma(0.01, 0.01), class = shape)),
                   seed = 10,
                   iter = 4000,
                   control = list(adapt_delta = 0.999),
                   cores = 4,
                   file = "model_fits/sens_new/m.fert.weak2")


#### Stronger prior  ------------------------------------------------------------

m.fert.strong <- brm(adult_Fertility ~ 0 + Intercept + 
                       edu_SchoolLeavingAge + 
                       s(edu_AdmissionYear)  +
                       (1 | edu_BirthCohort) +
                       (1 | match_fam_id),
                     data = subset(edu, edu_LeaveIslands == 0),
                     family = "negbinomial",
                     prior = c(
                       prior(normal(0, 0.05), class = "b"),
                       prior(student_t(3, 0, 2.5), class = "sd"),
                       prior(student_t(3, 0, 5.9), class = "sds"),
                       prior(gamma(0.01, 0.01), class = shape)),
                     seed = 10,
                     iter = 4000,
                     control = list(adapt_delta = 0.999),
                     cores = 4,
                     file = "model_fits/sens_new/m.fert.strong2")


## Lifespan  --------------------------------------------------------------------


### Basic model ----------------------------------------------------------------
#### Weaker prior  --------------------------------------------------------------

m.lifespan.weak <- brm(data = lifespan.data2.pp,
                       family = binomial,
                       event | trials(1) ~ 0 + period.f  + edu_SchoolLeavingAge + s(edu_AdmissionYear) + (1|edu_BirthCohort) +
                         (1 | match_fam_id),
                       prior = c(prior(normal(0, 1.5), class = "b", coef = "edu_SchoolLeavingAge"),
                                 prior(normal(0, 1.5), class = "b", coef = "sedu_AdmissionYear_1"),
                                 prior(normal(0, 1.5), class = "b"),
                                 prior(student_t(3, 0, 2.5), class = "sd"),
                                 prior(student_t(3, 0, 5.9), class = "sds")),
                       cores = 4, iter = 2000, warmup = 1000,
                       control = list(adapt_delta = 0.999),
                       seed = 10,
                       file = "model_fits/sens_new/m.lifespan.weak2")


#### Stronger prior  ------------------------------------------------------------

m.lifespan.strong <- brm(data = lifespan.data2.pp,
                         family = binomial,
                         event | trials(1) ~ 0 + period.f  + edu_SchoolLeavingAge + s(edu_AdmissionYear) + (1|edu_BirthCohort) +
                           (1 | match_fam_id),
                         prior = c(prior(normal(0, 0.15), class = "b", coef = "edu_SchoolLeavingAge"),
                                   prior(normal(0, 0.15), class = "b", coef = "sedu_AdmissionYear_1"),
                                   prior(normal(0, 1.5), class = "b"),
                                   prior(student_t(3, 0, 2.5), class = "sd"),
                                   prior(student_t(3, 0, 5.9), class = "sds")),
                         cores = 4, iter = 2000, warmup = 1000,
                         control = list(adapt_delta = 0.999),
                         seed = 10,
                         file = "model_fits/sens_new/m.lifespan.strong2")




### Full model -----------------------------------------------------------------
#### Weaker prior  --------------------------------------------------------------

m.lifespan.int.weak <- brm(data = lifespan.data2.pp,
                           family = binomial,
                           event | trials(1) ~ 0 + period.f  + edu_SchoolLeavingAge + edu_AdmissionYear + edu_AdmissionYear:edu_SchoolLeavingAge + (1|edu_BirthCohort) +
                             (1 | match_fam_id),
                           prior = c(prior(normal(0, 1.5), class = "b", coef = "edu_SchoolLeavingAge"),
                                     prior(normal(0, 1.5), class = "b", coef = "edu_AdmissionYear"),
                                     prior(normal(0, 1.5), class = "b", coef = "edu_SchoolLeavingAge:edu_AdmissionYear"),
                                     prior(normal(0, 1.5), class = "b"),
                                     prior(student_t(3, 0, 2.5), class = "sd")),
                           cores = 4, iter = 100000, warmup = 1000,
                           control = list(max_treedepth = 15),
                           init = 0,
                           seed = 10,
                           file = "model_fits/sens_new/m.lifespan.int.weak2")


#### Stronger prior  ------------------------------------------------------------

m.lifespan.int.strong <- brm(data = lifespan.data2.pp,
                             family = binomial,
                             event | trials(1) ~ 0 + period.f  + edu_SchoolLeavingAge + edu_AdmissionYear + edu_AdmissionYear:edu_SchoolLeavingAge + (1|edu_BirthCohort) +
                               (1 | match_fam_id),
                             prior = c(prior(normal(0, 0.15), class = "b", coef = "edu_SchoolLeavingAge"),
                                       prior(normal(0, 0.15), class = "b", coef = "edu_AdmissionYear"),
                                       prior(normal(0, 0.15), class = "b", coef = "edu_SchoolLeavingAge:edu_AdmissionYear"),
                                       prior(normal(0, 1.5), class = "b"),
                                       prior(student_t(3, 0, 2.5), class = "sd")),
                             cores = 4, iter = 100000, warmup = 1000,
                             control = list(max_treedepth = 15),
                             init = 0,
                             seed = 10,
                             file = "model_fits/sens_new/m.lifespan.int.strong2")




