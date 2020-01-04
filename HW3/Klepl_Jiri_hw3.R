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

Caravan.sample <- sample(nrow(Caravan))

Caravan.test <- Caravan[Caravan.sample,][1:1000,]
Caravan.train <- Caravan[Caravan.sample,][1001:nrow(Caravan),]

do.folds <- function(attempts, folds, type, first, second) {
    aucs <- c(1:(folds*attempts))

    for (n in 1:attempts) {
        Caravan.train.sample <- Caravan.train[sample(nrow(Caravan.train)),]

        answered.yes <- Caravan.train.sample[Caravan.train.sample$Purchase == "Yes",]
        answered.no <- Caravan.train.sample[Caravan.train.sample$Purchase == "No",]

        yes.count <- floor(nrow(answered.yes)/folds)
        no.count <- floor(nrow(answered.no)/folds)

        for (i in 1:folds) {
            train <- rbind(
                answered.yes[1:(folds*yes.count),][-c(((i-1)*yes.count+1):(i*yes.count)),],
                answered.no[1:(folds*no.count),][-c(((i-1)*no.count+1):(i*no.count)),])

            test <- rbind(
                answered.yes[((i-1)*yes.count+1):(i*yes.count),],
                answered.no[((i-1)*no.count+1):(i*no.count),])

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
                    mtry    = round(second)) # rounding forces integer

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
            fold.prediction <- prediction(predicted, targets)

            auc <- performance(
                fold.prediction,
                measure = "auc",
                fpr.stop = 0.2)@y.values[[1]]

            # roc <- performance(
            #     fold.prediction,
            #     measure = "tpr",
            #     x.measure = "fpr")

            aucs[(n - 1) * folds + i] <- auc
        }
    }

    aucs.mean <- mean(aucs)
    aucs.dev <- sd(aucs)

    if (all(abs(aucs - aucs.mean) < 0.0001)) {
		aucs.t.test <- data.frame("conf.int" = c(aucs.mean, aucs.mean))
    } else {
        aucs.t.test <- t.test(aucs, mu = aucs.mean, conf.level = 0.95)
    }

    # message(
    #     "Mean: ", aucs.mean, "\n",
    #     "Std. dev: ", aucs.dev, "\n",
    #     "Confidence interval: ", aucs.t.test$conf.int[1], ", ", aucs.t.test$conf.int[2])

    return(c(aucs.mean, aucs.dev, aucs.t.test$conf.int[1], aucs.t.test$conf.int[2]))
}

pdf("graphs.pdf")

test.trees <- function() {
    cps <- 10^seq(-2.155,-4, length=15)
    aucs <- sapply(cps, function(i) do.folds(10, 10, "tree", i)) # doing the experiment 10 times for each cp
    plotCI(
        x = cps,
        y = aucs[1,],
        ylab = "AUC_0.2",
        xlab = "cp",
        uiw = aucs[4,] - aucs[1,],
        liw = aucs[1,] - aucs[3,],
        err = "y",
        pch = 20,
        slty = 3,
        scol = "black",
        add = TRUE,
        log = "x")
}

dev.off()
