library(tidyverse)
library(ggfortify)
library(lme4)
library(lmerTest)
library(car)

rm(list = ls())

dat <- ...

dat$beighton_gr <- factor(dat$beighton_gr, levels = c("No Laxity", "Mild Laxity", "Hyperlaxity"))
dat$dysp <- factor(dat$dysp, levels = c("No Dysplasia", "BL Dysplasia", "Dysplasia"))
  
dat_male <- dat %>% 
  filter(sex == 0)

dat_fem <- dat %>% 
  filter(sex == 1)

                                     ## In-Clinic Models ## 

# Start Force FULL  

t.test(start_force ~ sex, data = dat)

full_start <- lmer(
  start_force ~ sex + age + bmi + 
    beighton_gr + flexion_in_clinic + er_in_clinic + ir_in_clinic + abd_in_clinic + 
    lce + sourcil + cotav + dysp + (1|pid), data = dat
)

summary(full_start)

full_start_sum <- round(as.data.frame(cbind(summary(full_start)$coefficients, as.data.frame(confint(full_start))[-c(1,2), ])), 5)

names(full_start_sum) <- c("Estimate", "Standard Error", "df", "t-value", "p-value", "95% CI (LB)", "95% CI (UB)")


# After Force FULL 
t.test(after_force ~ sex, data = dat)

full_after <- lmer(
  after_force ~ age + bmi + 
    beighton_gr + flexion_in_clinic + er_in_clinic + ir_in_clinic + abd_in_clinic + 
    lce + sourcil + cotav + dysp + (1|pid), data = dat_male
)

summary(full_after)

full_after_sum <- round(as.data.frame(cbind(summary(full_after)$coefficients, as.data.frame(confint(full_after))[-c(1,2), ])), 5)

names(full_after_sum) <- c("Estimate", "Standard Error", "df", "t-value", "p-value", "95% CI (LB)", "95% CI (UB)")


# All Vars + Males: START
start_mod1_m <- lmer(
  start_force ~ age + bmi + 
    beighton_gr + flexion_in_clinic + er_in_clinic + ir_in_clinic + abd_in_clinic + 
    lce + sourcil + cotav + dysp + (1|pid), 
  data = dat_male
)

summary(start_mod1_m)

length(residuals(start_mod1_m))

start_mod1_m_sum <- round(as.data.frame(cbind(summary(start_mod1_m)$coefficients, as.data.frame(confint(start_mod1_m))[-c(1,2), ])), 5)

start_mod1_m_sum <- start_mod1_m_sum 

names(start_mod1_m_sum) <- c("Estimate", "Standard Error", "df", "t-value", "p-value", "95% CI (LB)", "95% CI (UB)")



# All Vars + Males: AFTER 
start_mod2_m <- lmer(
  after_force ~ age + bmi + 
    beighton_gr + flexion_in_clinic + er_in_clinic + ir_in_clinic + abd_in_clinic + 
    lce + sourcil + cotav + dysp + (1|pid), 
  data = dat_male
)

summary(start_mod2_m)

length(residuals(start_mod2_m))

start_mod2_m_sum <- round(as.data.frame(cbind(summary(start_mod2_m)$coefficients, as.data.frame(confint(start_mod2_m))[-c(1,2), ])), 5)

start_mod2_m_sum <- start_mod2_m_sum  

names(start_mod2_m_sum) <- c("Estimate", "Standard Error", "df", "t-value", "p-value", "95% CI (LB)", "95% CI (UB)")


# All Vars + Females: START 
start_mod1_f <- lmer(
  start_force ~ age + bmi + 
    beighton_gr + flexion_in_clinic + er_in_clinic + ir_in_clinic + abd_in_clinic + 
    lce + sourcil + cotav + dysp + (1|pid), 
  data = dat_fem
  )

summary(start_mod1_f)

length(residuals(start_mod1_f))

start_mod1_f_sum <- round(as.data.frame(cbind(summary(start_mod1_f)$coefficients, as.data.frame(confint(start_mod1_f))[-c(1,2), ])), 5)

start_mod1_f_sum <- start_mod1_f_sum 

names(start_mod1_f_sum) <- c("Estimate", "Standard Error", "df", "t-value", "p-value", "95% CI (LB)", "95% CI (UB)")


# All Vars Female: AFTER 
start_mod2_f <- lmer(
  after_force ~ age + bmi + 
    beighton_gr + flexion_in_clinic + er_in_clinic + ir_in_clinic + abd_in_clinic + 
    lce + sourcil + cotav + dysp + (1|pid), 
  data = dat_fem
  )

summary(start_mod2_f)

length(residuals(start_mod2_f))

start_mod2_f_sum <- round(as.data.frame(cbind(summary(start_mod2_f)$coefficients, as.data.frame(confint(start_mod2_f))[-c(1,2), ])), 5)

start_mod2_f_sum <- start_mod2_f_sum

names(start_mod2_f_sum) <- c("Estimate", "Standard Error", "df", "t-value", "p-value", "95% CI (LB)", "95% CI (UB)")




