data <- read.csv("modeling_scored.csv", stringsAsFactors = FALSE)
data$Incident.ZIP.Postal <- as.factor(data$Incident.ZIP.Postal)
data$Gender <- as.factor(data$Gender)
library(tidyverse)


#### Forward Selection
library(boot)


null_mod <- glm(man.od ~  1,
                family = "binomial",
                data = data)

full_mod <- glm(man.od ~  Incident.ZIP.Postal + Patient.Age + pi_score + cc_w_score +
                  cn_w_score + cn_bg_score + cn_tg_score + cn_d_score,
                family = "binomial",
                data = data)

step(null_mod, scope = list(lower = null_mod, upper = full_mod), direction = "forward")


forward_mod <- glm(man.od ~ cn_d_score + cn_w_score + pi_score, 
                   family = "binomial", 
                   data = data)


x <- summary(forward_mod)

mod <- x$coefficients

write.csv(mod, "logistic_coeff.csv")

cv_error_forward <- cv.glm(data, forward_mod, K = 10)
cv_error_forward$delta[2]

####

#### Best Subsets
library(glmulti)

bss_mod <-
  glmulti(man.od ~  Incident.ZIP.Postal + Patient.Age + pi_score + cc_w_score +
            cn_w_score + cn_bg_score + cn_tg_score + cn_d_score, data = data,
          level = 1,               # No interaction considered
          method = "h",            # Exhaustive approach
          crit = "aic",            # AIC as criteria
          confsetsize = 5,         # Keep 5 best models
          plotty = T, report = T,  # No plot or interim reports
          fitfunction = "glm",     # glm function
          family = binomial)       # binomial family for logistic regression

summary(bss_mod@objects[[1]])

bss_mod <- glm(man.od ~ pi_score + cn_w_score + cn_bg_score + cn_tg_score + cn_d_score,
               family = "binomial",
               data = data)

cv_error_bss <- cv.glm(data, bss_mod, K = 10)
cv_error_bss$delta[2]

####

#### Lasso

####

#### Ridge

####

#### SVM
install.packages("e1071")
library(e1071)

data_svm_subset <- data %>% 
  select(man.od, Incident.ZIP.Postal, Patient.Age, pi_score, cc_w_score, 
         cn_w_score, cn_bg_score, cn_tg_score, cn_d_score)

svm_mod <- tune(svm, man.od ~ ., 
                kernel = "linear",
                ranges = list(
                  cost = c(0.0001, 0.005)),
                scale = FALSE,
                data = data_svm_subset)

summary(svm_mod)

svm_modp <- tune(svm, man.od ~ ., 
                kernel = "polynomial",
                ranges = list(
                  cost = c(0.005, 0.01),
                  degree = c(2, 3, 4)),
                scale = FALSE,
                data = data_svm_subset)

summary(svm_modp)

#svm_error <- summary(svm_mod)$performances[1, 2]

poly_svm <- svm(man.od ~.,
                  kernel = "polynomial",
                  cost = 0.005,
                  degree = 3,
                  scale = FALSE,
                  data = data_svm_subset)

####

#### Boosted Random Forest

####
