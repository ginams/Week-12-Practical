### Write you code here - use rick data set
# how to select best supset? USE cross-validation
# select model that has one parameter and compute r^2, select another model and computer its r^2 etc. 
# so you can find the best subset that predicts what you are interested in - here is richness

#repeat k iteration for the each replication of selecting one chunk for testing - this has more steps 

library(tidyverse)

survey_data = read_csv("RIKZ.csv")
head(survey_data)

richness = (survey_data[1:45,2:76]>0) %>% apply(1, sum)
richness
 
survey_data["richness"] = richness
survey_data

dim(survey_data)
sum(is.na(survey_data$richness))

survey_data_richness = survey_data[, 77:90]
head(survey_data_richness)

 library(leaps)

regfit.full <- regsubsets(richness ~., data = survey_data_richness, nvmax = 13)
reg.summary <- summary(regfit.full)
reg.summary

names(reg.summary)

reg.summary$rsq

# Plot RSS, adjusted R^2, Cp & BIC for all of the models at once 

par(mfrow = c(2, 2))

##RSS
plot(reg.summary$rss , xlab = "Number of Variables", ylab = "RSS", type = "l")

##ajusted R^2
plot(reg.summary$adjr2 , xlab = "Number of Variables", ylab = "Adjusted RSq", type = "l")

##Cp
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")

##BIC
plot(reg.summary$bic , xlab = "Number of Variables", ylab = "BIC", type = "l")



##Ajusted R^2
which.max (reg.summary$adjr2)

plot(reg.summary$adjr2, xlab = "Number of Variables", ylab = "adjr2", type = "l")
    
points (4, reg.summary$adjr2 [4] , col = "red", cex = 2, pch = 20)


##Cp
which.min(reg.summary$cp) 
    
plot(reg.summary$cp, xlab = "Number of Variables", ylab = "Cp", type = "l")
    points (3, reg.summary$cp[3] , col = "red", cex = 2, pch = 20)

##BIC
which.min(reg.summary$bic)

plot(reg.summary$bic , xlab = "Number of Variables", ylab = "BIC", type = "l")
    points (3, reg.summary$bic [3], col = "red", cex = 2, pch = 20)

par(mfrow = c(2, 2))

plot(regfit.full , scale = "r2")
plot(regfit.full , scale = "adjr2")
plot(regfit.full , scale = "Cp")
plot(regfit.full , scale = "bic")



coef(regfit.full, 3)

set.seed(1)
train <- sample(c(TRUE,FALSE), nrow(survey_data_richness), replace=TRUE)
test <- (!train)

regfit.best <- regsubsets(richness ~., 
    data = survey_data_richness[train, ], nvmax = 13)


test.mat <- model.matrix(richness ~., data = survey_data_richness[test, ])
    

val.errors <- rep(NA, 13)
for (i in 1:13) {
 coefi <- coef(regfit.best , id = i)
 pred <- test.mat[, names(coefi)] %*% coefi
 val.errors[i] <- mean((survey_data_richness$richness[test] - pred)^2)
}

val.errors

which.min(val.errors)

coef(regfit.best, 3)

predict.regsubsets <- function(object, newdata, id, ...) {
    form <- as.formula(object$call[[2]])
    mat <- model.matrix(form, newdata)
    coefi <- coef(object, id=id)
    xvars <- names(coefi)
    mat[, xvars] %*% coefi
}

regfit.best <- regsubsets(richness~., data=survey_data_richness, nvmax=13)
coef(regfit.best, 3)



k <- 10
n <- nrow(survey_data_richness)
set.seed(1)
folds <- sample(rep(1:k, length = n))
cv.errors <- matrix(NA, k, 13,
    dimnames = list(NULL, paste(1:13)))

for (j in 1:k) {
    best.fit <- regsubsets(richness~ .,
        data = survey_data_richness[folds != j, ],
        nvmax = 13)
    for (i in 1:13) {
        pred <- predict(best.fit , survey_data_richness[folds == j, ], id = i)
        cv.errors [j, i] <-
        mean (( survey_data_richness$richness[folds == j] - pred)^2)
     }
 }

best.fit

pred


mean.cv.errors <- apply(cv.errors, 2, mean)
mean.cv.errors

reg.best <- regsubsets(richness ~., data=survey_data_richness,
    nvmax = 13)
    coef(reg.best, 10)
