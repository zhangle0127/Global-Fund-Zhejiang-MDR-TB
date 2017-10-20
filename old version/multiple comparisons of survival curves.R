# multiple comparisons of survival curves

New function pairwise_survdiff() added.

Survival curves with global p-value
library(survival)
library(survminer)
# Survival curves with global p-value
data(myeloma)
fit <- survfit(Surv(time, event) ~ molecular_group, data = myeloma)
ggsurvplot(fit, legend.labs = levels(myeloma$molecular_group),
           pval = TRUE)