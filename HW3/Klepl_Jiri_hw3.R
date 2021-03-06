library(ISLR)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ROCR)
library(glmnet)
library(plotrix)
library(grid)
library(gridExtra)

# Task 1

first.task <- function() {
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

    pdf("table-moshoofd.pdf")
    grid.table(t(moshoofd))
    dev.off()

    # ---

    mostype.numbers <- table(Caravan[,c(1)])
    mostype.purchased <- round(table(Caravan[,c(1,86)])[,2] / table(Caravan[,c(1)]),4) * 100

    mostype <- data.frame(
        "Number" = as.numeric(mostype.numbers),
        "Rate" = as.numeric(mostype.purchased))

    row.names(mostype) <- row.names(mostype.numbers)

    pdf("table-mostype.pdf", width = 20)
    grid.table(t(mostype))
    dev.off()

    # 1b

    mostype.vs.moshoofd <- table(Caravan[,c(1,5)])

    pdf("table-mostype-vs-moshoofd.pdf", width = 17)
    grid.table(t(mostype.vs.moshoofd))
    dev.off()
}

# Task 2

set.seed(123)

Caravan.sample <- sample(nrow(Caravan))

Caravan.test <- Caravan[Caravan.sample,][1:1000,]
Caravan.train <- Caravan[Caravan.sample,][1001:nrow(Caravan),]

pred.evaluate <- function(predicted, targets, fpr.stop, do.roc) {
    pred <- prediction(predicted, targets)

    auc <- performance(
        pred,
        measure = "auc",
        fpr.stop = fpr.stop)@y.values[[1]]

    if (do.roc) {
        roc <- performance(
            pred,
            measure = "tpr",
            x.measure = "fpr")

        return(c(auc,roc))
    } else {
        return(c(auc,NULL))
    }
}

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
                    lambda = first,
                    alpha = second)
                predicted <- predict(
                    model,
                    as.matrix(test[,1:85]),
                    type="response")
            }

            aucs[(n - 1) * folds + i] <- pred.evaluate(
                predicted,
                ifelse(test$Purchase == "Yes", 1, 0),
                0.2,
                FALSE)[1]
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



test.tree <- function() {
    pdf("tree.pdf")

    cps <- 10^seq(-2,-4, length=25)
    aucs <- sapply(cps, function(i) do.folds(10, 10, "tree", i))
    # doing the experiment 10 times for each cp
    # (this gives us more consistent results)

    plotCI(
        x = log(cps, 10),
        y = aucs[1,],
        ylab = "AUC_0.2",
        xlab = "log_10 cp",
        uiw = aucs[4,] - aucs[1,],
        liw = aucs[1,] - aucs[3,],
        err = "y",
        pch = 20,
        slty = 3,
        scol = "black",
        add = FALSE)

    dev.off()
}
# last measured 'sweet' cp was 0.001211527

test.forest <- function() {
    pdf("forest.pdf")

    for (mtry in 5^seq(1, log(85/3, 5), length = 5)) {
        ntree <- 10^seq(1, 3, length = 10)

        aucs <- sapply(ntree, function(n) do.folds(1, 10, "forest", n, mtry))

        plotCI(
            x = ntree,
            y = aucs[1,],
            ylab = "AUC_0.2",
            xlab = "ntree",
            main = paste("mtry =", mtry),
            uiw = aucs[4,] - aucs[1,],
            liw = aucs[1,] - aucs[3,],
            err = "y",
            pch = 20,
            slty = 3,
            scol = "black",
            add = FALSE)
    }

    dev.off()
}
# this method seems to be unfit for this problem (or at least difficult to tune)
# pretty good pair of parameters seems to be mtry = 18, ntree = 600

test.regression <- function() {
    pdf("regression.pdf")

    for (alpha in seq(0, 1, length = 10)) {
        if (alpha >= 0.5) {
            lambda <- 10^seq(-2.3, -1, length = 15)
        } else if(alpha >= 0.1) {
            lambda <- 10^seq(-1.5, -1, length = 15)
        } else {
            lambda <- 10^seq(-0.9, -0.4, length = 10)
        }

        if (alpha >= 0.4) {
            aucs <- sapply(lambda, function(l) do.folds(10, 10, "regression", l, alpha))
            # computationally we can afford more attempts
        } else {
            aucs <- sapply(lambda, function(l) do.folds(1, 10, "regression", l, alpha))
        }

        plotCI(
            x = log(lambda, 10),
            y = aucs[1,],
            ylab = "AUC_0.2",
            xlab = "log_10 lambda",
            main = paste("alpha =", alpha),
            uiw = aucs[4,] - aucs[1,],
            liw = aucs[1,] - aucs[3,],
            err = "y",
            pch = 20,
            slty = 3,
            scol = "black",
            add = FALSE)
    }

    dev.off()
}
# The 'sweet' (lambda, alpha) pair seems to be:
# lambda = 0.014597743
# alpha = 0.777...
# (according to the last experiment)

# Task 2a,b,c

# test.tree()
# test.forest()
# test.regression()

# Task 2d

# decision tree parameters
sweet.cp <- 0.001211527

# random forest parameters
sweet.ntree <- 600
sweet.mtry <- 18

# logistic regression parameters
sweet.lambda <- 0.014597743
sweet.alpha <- 7/9

models.evaluate <- function() {
    pdf("evaluation.pdf")

    message("Training decision tree")
    model.tree <- rpart(
        formula = Purchase ~ .,
        data    = Caravan.train,
        method  = "class",
        cp      = sweet.cp)

    predicted.tree <- predict(
        model.tree,
        newdata = Caravan.test,
        type = "prob")[,2]

    message("Training random forest")
    model.forest <- randomForest(
        formula = Purchase ~ .,
        data    = Caravan.train,
        ntree   = round(sweet.ntree), # rounding forces integer
        mtry    = round(sweet.mtry)) # rounding forces integer

    predicted.forest <- predict(
        model.forest,
        newdata = Caravan.test,
        type="prob")[,2]

    message("Training logistic regression")
    model.regression <- glmnet(
        as.matrix(Caravan.train[,1:85]), # all but 'Purchase'
        as.matrix(ifelse(Caravan.train$Purchase == "Yes", 1, 0)),
        family = "binomial",
        lambda = sweet.lambda,
        alpha = sweet.alpha)

    predicted.regression <- predict(
        model.regression,
        as.matrix(Caravan.test[,1:85]),
        type="response")

    targets <- ifelse(Caravan.test$Purchase == "Yes", 1, 0)

    result <- pred.evaluate(predicted.tree, targets, 0.2, TRUE)
    plot(
        result[[2]],
        main = paste(
            "Decision tree\ncp = ",
            sweet.cp,
            ", AUC_0.2 = ",
            result[[1]],
            sep = ""))

    result <- pred.evaluate(predicted.forest, targets, 0.2, TRUE)
    plot(
        result[[2]],
        main = paste(
            "Random forest\nntree = ",
            sweet.ntree,
            ", mtry = ",
            sweet.mtry,
            ", AUC_0.2 = ",
            result[[1]],
            sep = ""))

    result <- pred.evaluate(predicted.regression, targets, 0.2, TRUE)
    plot(
        result[[2]],
        main = paste(
            "Logistic regression\nlambda = ",
            sweet.lambda,
            ", alpha = ",
            sweet.alpha,
            ", AUC_0.2 = ",
            result[[1]],
            sep = ""))

    dev.off()
}
# this evaluation suggests choosing the logistic regression model
# as this gives slightly worse than the decision tree but gives us
# almost unique probabilities for each customer

# Task 2d,e
find.threshold <- function() {
    model.regression <- glmnet(
        as.matrix(Caravan.train[,1:85]), # all but 'Purchase'
        as.matrix(ifelse(Caravan.train$Purchase == "Yes", 1, 0)),
        family = "binomial",
        lambda = sweet.lambda,
        alpha = sweet.alpha)

    predicted.regression <- predict(
        model.regression,
        as.matrix(Caravan.test[,1:85]),
        type="response")

    names(predicted.regression) <- rownames(Caravan.test)

    regression.hundred <- sort(predicted.regression, TRUE)[1:100]


    threshold <- regression.hundred[100]

    message("Optimal threshold seems to be ", threshold)

    return(threshold)
}

# Task 3
features.evaluate <- function() {
    message("Training decision tree")
    model.tree <- rpart(
        formula = Purchase ~ .,
        data    = Caravan.train,
        method  = "class",
        cp      = sweet.cp)

    message("Training random forest")
    model.forest <- randomForest(
        formula = Purchase ~ .,
        data    = Caravan.train,
        ntree   = round(sweet.ntree), # rounding forces integer
        mtry    = round(sweet.mtry)) # rounding forces integer

    forest.importance <- importance(model.forest)[,1]

    message("Training lasso")
    model.lasso <- glmnet(
        as.matrix(Caravan.train[,1:85]), # all but 'Purchase'
        as.matrix(ifelse(Caravan.train$Purchase == "Yes", 1, 0)),
        family = "binomial",
        lambda = sweet.lambda,
        alpha = 1)

	lasso.features <- coef(model.lasso)[2:86,1]

    tree.used <- model.tree$variable.importance[names(forest.importance)]
    names(tree.used) <- names(forest.importance)
    tree.used[is.na(tree.used)] <- 0

    comparison <- data.frame(
        "decision.tree" = tree.used,
        "random.forest" = forest.importance,
        "lasso" = lasso.features)

    print(comparison)

    message("correlation of feature importance values according to the models")
    print(cor(comparison))
    # we can see significant correlation between
    # the decision tree model and the random forest model
    # but not so much between the lasso model and either of these

    # graphically:
    pdf("comparison.pdf")
    plot(comparison)
    dev.off()
}

# Task 4
final.evaluation <- function() {
    threshold <- find.threshold()

    model.regression <- glmnet(
        as.matrix(Caravan[,1:85]), # all but 'Purchase'
        as.matrix(ifelse(Caravan$Purchase == "Yes", 1, 0)),
        family = "binomial",
        lambda = sweet.lambda,
        alpha = sweet.alpha)

    final.test <- read.csv("caravan.test.1000.csv", header = FALSE, sep='\t')

    predicted.regression <- predict(
        model.regression,
        as.matrix(final.test[,1:85]),
        type="response")

    names(predicted.regression) <- rownames(final.test)

    regression.hundred <- sort(predicted.regression, TRUE)[1:100]

    predicted.regression[names(regression.hundred)] <- 1
    predicted.regression[-as.numeric(names(regression.hundred))] <- 0

    write(predicted.regression, "T.prediction", ncolumns=1)

    return(as.numeric(names(regression.hundred)))
}
