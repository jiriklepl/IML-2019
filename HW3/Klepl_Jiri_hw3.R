library(ISLR)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)
library(glmnet)
library(plotrix)
# str(Caravan)

# Task 1

message("Distribution:")

print(table(Caravan$Purchase) / nrow(Caravan))

set.seed(123)
Caravan.sample <- Caravan[sample(nrow(Caravan))[1:100],]$Purchase

# 1a

moshoofd.numbers <- table(Caravan[,c(5)])
moshoofd.purchased <- round(table(Caravan[,c(5,86)])[,2] / table(Caravan[c(5)]),4) * 100

moshoofd <- data.frame(
    "Number" = as.numeric(moshoofd.numbers),
    "Rate" = as.numeric(moshoofd.purchased))

message("MOSHOOFD:")
print(moshoofd)

# ---

mostype.numbers <- table(Caravan[,c(1)])
mostype.purchased <- round(table(Caravan[,c(1,86)])[,2] / table(Caravan[,c(1)]),4) * 100

mostype <- data.frame(
    "Number" = as.numeric(mostype.numbers),
    "Rate" = as.numeric(mostype.purchased))

row.names(mostype) <- row.names(mostype.numbers)

message("MOSTYPE:")
print(mostype)

# 1b

mostype.vs.moshoofd <- table(Caravan[,c(1,5)])

message("MOSTYPE vs MOSHOOFD:")
print(mostype.vs.moshoofd)

# Task 2

set.seed(123)

do.folds <- function(folds, type, first, second) {
    Caravan.sample <- sample(nrow(Caravan))

    Caravan.test <- Caravan[Caravan.sample,]

    answered.yes <- Caravan.test[Caravan.test[,86] == "Yes",]
    answered.no <- Caravan.test[Caravan.test[,86] == "No",]

    yes.count <- floor(nrow(answered.yes)/folds)
    no.count <- floor(nrow(answered.no)/folds)

    aucs <- c(1:10)

    for (i in 1:folds) {
        yes.test <- answered.yes[((i-1)*yes.count+1):(i*yes.count),]
        no.test <- answered.no[((i-1)*no.count+1):(i*no.count),]

        yes.rest <- answered.yes[1:(folds*yes.count),][-c(((i-1)*yes.count+1):(i*yes.count)),]
        no.rest <- answered.no[1:(folds*no.count),][-c(((i-1)*no.count+1):(i*no.count)),]

        train <- rbind(yes.rest, no.rest)
        test <- rbind(yes.test, no.test)

        if (type == "tree") {
            model <- rpart(
                formula = Purchase ~ .,
                data    = train,
                method  = "class",
                cp      = first)

            predicted <- predict(
                model,
                newdata = test,
                type = "prob")[,2]
        } else if (type == "forest") {
            model <- randomForest(
                formula = Purchase ~ .,
                data    = train,
                ntree   = round(first), # rounding forces integer
                mtry    = round(second) # rounding forces integer
                )

			predicted <- predict(
                model,
                newdata = test,
                type="prob")[,2]
        } else if (type == "regression") {
			model <- glmnet(
                as.matrix(train[,1:85]), # all but 'Purchase'
                as.matrix(ifelse(train$Purchase == "Yes", 1, 0)),
                family = "binomial",
                lambda = 10^seq(1, -4, length = first),
                alpha = second)

            predicted <- predict(
                model,
                as.matrix(test[,1:85]),
                type="response")[,2]
        }

        targets <- ifelse(test$Purchase == "Yes", 1, 0)
        fold.prediction <- prediction(predicted,targets)

        auc <- performance(
            fold.prediction,
            measure = "auc")@y.values[[1]]
        roc <- performance(
            fold.prediction,
            measure = "tpr",
            x.measure = "fpr")

        aucs[i] <- auc
    }

    aucs.mean <- mean(aucs)
    aucs.dev <- sd(aucs)

    aucs.t.test <- t.test(aucs, mu=aucs.mean, conf.level= 0.95)

    message(
        "Mean: ", aucs.mean, "\n",
        "Std. dev: ", aucs.dev, "\n",
        "Confidence interval: ", aucs.t.test$conf.int[1], ", ", aucs.t.test$conf.int[2]
    )
}

test.trees <- function() {
    for (i in 1:30) {
        message(i)
        do.folds(10, "tree", i * 0.0001)
    }
}
