library(ISLR)
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

Caravan.test <- Caravan[Caravan.sample[1:1000],]
Caravan.train <- Caravan[Caravan.sample[-c(1:1000)],]




answered.yes <- Caravan.train[Caravan.train[,86] == "Yes",]
answered.no <- Caravan.train[Caravan.train[,86] == "No",]


message("Let's fold:")
for (i in 1:6) {
    yes.count = floor(nrow(answered.yes)/6)
    no.count = floor(nrow(answered.no)/6)

    yes.test <- answered.yes[((i-1)*yes.count+1):(i*yes.count),]
    no.test <- answered.no[((i-1)*no.count+1):(i*no.count),]

    yes.rest <- answered.yes[1:(6*yes.count),][-c(((i-1)*yes.count+1):(i*yes.count)),]
    no.rest <- answered.no[1:(6*no.count),][-c(((i-1)*no.count+1):(i*no.count)),]
}