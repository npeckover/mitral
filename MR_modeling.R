####--- Load libraries and select relevant variables ---####
library(caret)
library(mice)
library(pROC)
library(PRROC)

# can we predict the severity of MR based on lab values, medications, history, and cath results?
# remove variables that are not relevant to this task
DF_MR <- DF %>%
  select(-c(DateOfEcho, PISARradius, EROA, VC, MR.Volume, ProcedureDate, SymptomOnsetDate,
            IndexedLAVolume, IndexedLVEDV, Date.of.death, PreviousLVEF, Preexisting, 
            TimeFromAngioToEcho..days., TimeFromSymptomOnsetToEcho.days., 
            Severity, Severity2, NTproBNP, RenalFunction, LVEF..., AgeAtTimeOfEcho,
            X30Death, X30DateOfDeath, X30TimeFromDischargeToDeath.days., X30TimeFromSymptomOnsetToDeath.days.,
            X12Death, X12DateOfDeath, X12TimeFromDischargeToDeath.days., X12TimeFromSymptomOnsetToDeath.days.))

####--- PRE-PROCESSING ---####

# create test/train split vector
set.seed(999991)
trainingIndex <- createDataPartition(DF_MR$Severity3, p = 0.75, list = F)
# create additional vector of T/F for imputation
TF <- rep(FALSE, 1000)
TF[-trainingIndex] <- print(TRUE)  # value of TRUE tells mice() to ignore the row for test-train split integrity

impute_model <- mice(DF_MR, 
                     seed = 9001, 
                     m = 20,         # large number of multiple imputations
                     ignore = TF)    # rows to ignore

DF_MRi <- complete(impute_model) # use the impute model on whole dataset to fix missing values

# test/train split
train <- DF_MRi[trainingIndex,]
test <- DF_MRi[-trainingIndex,]

# create custom SMOTE parameters
smote300.150 <- list(name = "Custom SMOTE params",
                func = function (x, y) {
                  library(DMwR)
                  dat <- if (is.data.frame(x)) x else as.data.frame(x)
                  dat$.y <- y
                  dat <- SMOTE(.y ~ ., data = dat, k = 5, perc.over = 300, perc.under = 150)
                  list(x = dat[, !grepl(".y", colnames(dat), fixed = TRUE)], 
                       y = dat$.y)
                },
                first = TRUE)

# set train controls:  10-fold CV and SMOTE sampling
ctrl <- trainControl(method = "cv",
                     number = 10,
                     classProbs = T,
                     sampling = smote300.150)

####--- XGBTrees ---####
xgbTree_grid <- expand.grid(nrounds = 25,
                            max_depth = 1,
                            eta = c(0.1, 0.2, 0.3),
                            gamma = 0,
                            colsample_bytree = c(0.5, 0.6, 0.7, 0.8),
                            min_child_weight = 1,
                            subsample = c(0.5, 0.6, 0.7))

set.seed(39921)
xgbT <- suppressWarnings(train(Severity3 ~ .,
                               data = train,
                               method = "xgbTree",
                               trControl = ctrl,
                               metric = "Kappa",
                               tuneGrid = xgbTree_grid))
plot(xgbT)

####--- Random Forest ---####
randforGrid <- expand.grid(mtry = c(6:12),
                           min.node.size = c(seq(from = 30, to = 200, by = 10)),
                           splitrule = c("extratrees")) # try gini for similar results (less likely to guess MODs)

set.seed(11681) 
randfor <- train(Severity3 ~ .,
                 data = train,
                 method = "ranger", 
                 trControl = ctrl,
                 metric = "Kappa",
                 tuneGrid = randforGrid,
                 verbose = F)

randfor
plot(randfor)

####--- ElasticNet Logistic Regression ---####
reglogitGrid <- expand.grid(alpha = c(0.5, 0.75, 1),
                            lambda = c(0.0001, 0.001, 0.01, 0.05, 0.1, 0.2))
set.seed(60801)
reglogit1 <- train(Severity3 ~ .,
                   data = train,
                   method = "glmnet",
                   family = "binomial",
                   trControl = ctrl,
                   tuneGrid = reglogitGrid,
                   verbose = F)

reglogit1
plot(reglogit1)

####--- Logistic Regression ---####
set.seed(774111)
logit1 <- glm(Severity3 ~ .,
              family = "binomial",
              data = train)
summary(logit1)

####---------- PREDICTIONS -----------####

# xgbT
xgbT_preds <- predict(xgbT, test)
xgbT_probs <- predict(xgbT, test, type = "prob")
confusionMatrix(xgbT_preds, test$Severity3, positive = "MODs", mode = "everything") 

# random forest
rf_preds <- predict(randfor, test)
rf_probs <- predict(randfor, test, type = "prob")
confusionMatrix(rf_preds, test$Severity3, positive = "MODs", mode = "everything") # 0.9679

# elastic net 
reglog_preds <- predict(reglogit1, test)
reglog_probs <- predict(reglogit1, test, type = "prob")
confusionMatrix(reglog_preds, test$Severity3, positive = "MODs", mode = "everything") # 0.7952

# logistic regression
logit_probs <- predict(logit1, test, type = "response")
logit_preds <- rep("MODs", 249)
logit_preds[logit_probs <= 0.2] <- "NONEm"  # changing threshold from 0.5 to 0.2 gives lower sens but high spec
logit_preds <- as.factor(logit_preds)
confusionMatrix(logit_preds, test$Severity3, positive = "MODs", mode = "everything") # 0.8996

####--- ROC ---####

# xgbT
roc_xgbT <- roc(test$Severity3, xgbT_probs$NONEm) # AUC: 0.7421
roc_xgbT

ggroc(roc_xgbT, color = "#77025e", size = 1) +
  geom_abline(intercept = 1, slope = 1, linetype = 2, alpha = 0.75) +
  theme_np()  + 
  theme(panel.grid.major = element_blank(),
        axis.title = element_text()) +
  labs(title = "ROC", subtitle = "Area Under the Curve = 0.7572", y = "Sensitivities", x = "Specificities")

# random forest
roc_rf <- roc(test$Severity3, rf_probs$NONEm) # AUC: 0.7173
roc_rf

# elastic net
roc_reglog <- roc(test$Severity3, reglog_probs$NONEm) # AUC: 0.6209
roc_reglog

# logistic regression
roc_logit <- roc(test$Severity3, logit_probs) # AUC: 0.6024
roc_logit

#### --- PR Curves ----####
PRC <- as.data.frame(pr.curve(xgbT_preds, test$Severity3, curve = T)[["curve"]])

ggplot(PRC) + 
  geom_step(aes(x = V1, y = V2), color = "#77025e", size = 1) + 
  geom_abline(intercept = 0.068273, slope = 0, linetype = 2, alpha = 0.75) +
  theme_np() + 
  theme(panel.grid.major = element_blank(),
                     axis.title = element_text()) +
  scale_y_continuous(limits = c(0,1)) + 
  scale_x_continuous(limits = c(0,1)) + 
  labs(title = "PR Curve", subtitle = "Area Under the Curve = 0.543", y = "Precision", x = "Recall")

####--- examining final results ----####
round(xgbT_probs[which(test$Severity3 == "MODs"), 2], 3)

results <- cbind(xgbT_probs, Severity = test$Severity3)

results %>% ggplot(aes(y = MODs, x = Severity)) +
  geom_boxplot()

varimp <- varImp(xgbT)
varimp