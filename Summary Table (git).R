## Summary ##
source("...")

library(tidyverse)
library(table1)

render.cont <- function(x) {
  with(stats.apply.rounding(stats.default(x), digits = 3, round.integers = F, digits.pct = 2), 
       c("", "Mean (SD)" = sprintf("%s (&plusmn; %s)", MEAN, SD)))
}

render.cat <- function(x) {
  c("", 
    sapply(stats.default(x), 
           function(y) with(y, sprintf("%d (%0.0f %%)", FREQ, PCT))))
}

pvalue <- function(x, ...) {
  y <- unlist(x)
  g <- factor(rep(1:length(x), times = sapply(x, length)))
  if (is.numeric(y)) {
    p <- t.test(y ~ g)$p.value
  } else {
    p <- chisq.test(table(y, g))$p.value
  }
  c("", sub("<", "&lt;", format.pval(p, digits = 3, eps = 0.001)))
}

dat.sum <- dat


dat.sum$sex <- factor(dat.sum$sex, labels = c("Male", "Female"))
label(dat.sum$sex) <- "Sex"
label(dat.sum$age) <- "Age"
label(dat.sum$bmi) <- "BMI"

dat.sum$dysp <- factor(dat.sum$dysp)
label(dat.sum$dysp) <- "Extent of Dysplasia"

dat.sum$bilateral <- factor(dat.sum$bilateral, labels = c("No", "Yes"))
label(dat.sum$bilateral) <- "Bilateral?"

label(dat.sum$beighton_gr) <- "Beighton (Grouped)"

label(dat.sum$flexion_in_clinic) <- "Flexion (In-Clinic)"
label(dat.sum$er_in_clinic) <- "External Rotation (In-Clinic)"
label(dat.sum$ir_in_clinic) <- "Internal Rotation (In-Clinic)"
label(dat.sum$abd_in_clinic) <- "Abduction (In-Clinic)"

dat.sum <- dat.sum %>% 
  select(
    !c(pid, side_of_scope)
  )

# Demographics 
names(dat.sum)

t_table1 <- table1(
  ~ age + bmi + flexion_in_clinic + er_in_clinic + ir_in_clinic + abd_in_clinic + lce + sourcil + version + torsion + cotav | sex, 
  data = dat.sum, 
  overall = F,
  extra.col=list(`P-value`= pvalue),
  render.continuous = render.cont, 
  render.categorical = render.cat,
  render.missing = NULL
)

t_table2 <- table1(
  ~ start_force + after_force | sex, 
  data = dat.sum, 
  overall = F,
  extra.col=list(`P-value`= pvalue),
  render.continuous = render.cont, 
  render.categorical = render.cat,
  render.missing = NULL
) 

chi_table <- table1(
  ~ dysp + beighton_gr + bilateral | sex, 
  data = dat.sum, 
  overall = F,
  extra.col=list(`P-value`= pvalue),
  render.continuous = render.cont, 
  render.categorical = render.cat,
  render.missing = NULL
)
