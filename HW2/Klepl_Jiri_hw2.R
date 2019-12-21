library('ISLR')
library('rpart')
library('rpart.plot')

# 1.1

model <- lm(mpg ~. -name, Auto)
message("1.1 results:")
print(summary(model))


# 1.2

sorted <- Auto[order(Auto$acceleration), ]
attach(sorted)

message('Printing to pdf...')
pdf('mpg_vs_acceleration.pdf')

plot(acceleration, mpg, main="ISLR: Auto data set (1.2)", 
	xlab = "Weight",
	ylab = "Miles Per Gallon",
	pch = 19,
	col = "black")

cols <- c("blue", "orange", "red", "green", "pink")

labels <- c()

message("1.2 results:")
for(i in 1:5) {
    pmodel <- lm(mpg ~ poly(acceleration, i), sorted)
    labels[i] <- paste("Degree", i, "(R^2=", summary(pmodel)$r.squared, ")")

    message(labels[i])
    points(acceleration, predict(pmodel), type="l", lwd=5, col = cols[i])
}
message("")

legend("topright",
	labels,
	col = cols,
	lty = c(1,1,1),
	lwd = c(5,5,5))

detach(sorted)

dev.off()

# 2.1

Auto01 <- Auto
med <- median(Auto01$mpg)

Auto01$mpg01 <- sapply(Auto01$mpg, function(val) ifelse(val >= med, 1, 0))

d <- Auto01[,2:ncol(Auto01)]
num.examples <- nrow(d)

message(paste("Entropy (2.1):", -sum(sapply(table(d$mpg01), function(x) x/num.examples * log2(x/num.examples)))))
message("")

# 2.2

set.seed(123)
s <- sample(num.examples)

num.train <- round(0.8 * num.examples)
num.test <- num.examples - num.train


training <- d[s[1:num.train],]
testing <- d[s[(num.train+1):num.examples],]

# 2.3

log_0 <- glm(mpg01 ~ -., data = training, family = binomial)
log_0$xlevels[["name"]] <- levels(testing$name)

prob_pred <- predict(log_0, type = 'response', newdata = testing)

# classfication rule
mpg01_pred <- ifelse(prob_pred >= 0.5, 1, 0)

# making the confusion matrix
cm <- table(testing$mpg01, mpg01_pred >= 0.5)

# accuracy
message(paste("Accuracy (2.3):", sum(diag(cm))/sum(cm)))
message("")

# 2.4

test_with_treshold <- function(treshold) {
    message(paste("Testing with treshold:", treshold))

    log_all <- glm(mpg01 ~ . -name, data = training, family = binomial)
    log_all$xlevels[["name"]] <- levels(testing$name)

    # 2.4a

    message(paste("Training error rate (2.4a):", sum(ifelse(training$mpg01 != ifelse(predict(log_all) >= treshold, 1, 0), 1, 0)) / num.train))

    # 2.4b

    prob_pred <- predict(log_all, type = 'response', newdata = testing)

    # classfication rule
    mpg01_pred <- ifelse(prob_pred >= treshold, 1, 0)

    # making the confusion matrix
    cm <- table(testing$mpg01, mpg01_pred >= treshold)

    # Printing results
    message(paste("Testing error rate (2.4b):", 1 - sum(diag(cm))/sum(cm)))
    message(paste("Testing sensitivity (2.4b):", cm[2,2]/sum(cm[2,])))
    message(paste("Testing specificity (2.4b):", cm[1,1]/sum(cm[1,])))

    #2.5a
    if (treshold != 0.5) {
        precision <- cm[2,2]/sum(cm[,2])
        recall <- cm[2,2]/sum(cm[2,])

        message(paste("Testing precision (2.5a):", precision))
        message(paste("Testing recall (2.5a):", recall))
        message(paste("Testing F1-measure (2.5a):", 2 * (precision * recall) / (precision + recall)))
    }

    message("")
}


# 2.4b
message("2.4c results:")
test_with_treshold(0.5)

# 2.5a
message("2.5 results:")
test_with_treshold(0.1)
test_with_treshold(0.9)


# 2.4c
print(summary(glm(mpg01 ~ . -name, data = training, family = binomial)))

# 2.6


message('Printing to pdf...')
pdf('tree_plot.pdf')

test_with_cp <- function(cp) {
    tmodel <- rpart(mpg01 ~ .-name, training, method = "class", cp = cp)
    rpart.plot(tmodel, main = paste("cp =", cp))

    message(paste("Training error rate (2.6a):", sum(ifelse(training$mpg01 != predict(tmodel, type = "class"), 1, 0)) / num.train))
    message(paste("Testing error rate (2.6a):", sum(ifelse(testing$mpg01 != predict(tmodel, newdata = testing, type = "class"), 1, 0)) / num.test))
    message("")
}

message("2.6b results:")

for(i in 0:20) {
    cp <- i * 0.05

    message(paste("cp:", cp))
    test_with_cp(cp)
}

dev.off()
