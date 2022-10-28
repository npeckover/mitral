#### ----- Survival Analysis ----- ####
# create Survival Analysis df
SURV <- data.frame(Severity = DF$Severity,
                   MIType = DF$MIType,
                   SymptOnset = totalDF$SymptomOnsetDate,
                   Death30 = totalDF$`30DateOfDeath`,
                   Death12 = totalDF$`12DateOfDeath`)
# combine the data from the two death cols
SURV$Death12[is.na(SURV$Death12)] <- SURV$Death30[is.na(SURV$Death12)] 
# create new indicator variable for death
SURV$Died <- ifelse(is.na(SURV$Death12), 0, 1)

# if did not die, fill in 'time' with max date.of.death (2019-12-21)
SURV$Death12 <- replace_na(SURV$Death12, as.Date("2019-12-21"))

# create time from symptom onset to death
SURV$Time <- as.numeric(SURV$Death12 - SURV$SymptOnset)

library(survival)
attach(SURV)
fit.surv <- survfit(Surv(Time, Died) ~ Severity)

ggsurv(fit.surv, 
       plot.cens = F,
       order.legend = F,
       surv.col = c("#f4ba55",  "#d26257", "#bc2354", "#77025e"),
       size.est = 1) + 
  theme_np() + 
  theme(panel.grid.major = element_blank(),
        axis.title = element_text(),
        legend.position = c(.9,.5),
        legend.direction = "vertical",
        legend.key = element_blank()) +
  scale_y_continuous(limits = c(0.55,1)) + 
  scale_x_continuous(limits = c(0,900)) +
  labs(title = "Estimated Probability of Survival", y = "Probability", x = "Days")

logrank.test <- survdiff(Surv(Time, Died) ~ Severity)
logrank.test

# cox proportional hazards models
fit.cox <- coxph(Surv(Time, Died) ~ Severity)
summary(fit.cox)