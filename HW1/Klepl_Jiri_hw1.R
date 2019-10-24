source("load-mov-data.R")


occurat <- table(examples[, c(3, 7)])
occupations <- table(examples[, c(7)])
ratings <- table(examples[, c(3)])

entropy <- 0

for (r in names(ratings)) {
    for (o in names(occupations)) {
        prob <- occurat[r, o] / sum(occurat)
        condprob <- occurat[r, o] / ratings[r]
        entropy_val <- prob * log2(condprob)
        entropy <- entropy - entropy_val
    }
}

print(paste('Entropy: ', as.numeric(entropy)))
print('', quote = FALSE)


movies <- table(examples[, c(1)])

print('Printing to pdf...')
pdf('boxplot.pdf')

ratings <- examples[sapply(examples[1], function(x) x %in% names(movies[movies == 67])), ][, c(1, 3, 9)]
boxplot(rating~movie, ratings, names = unique(ratings$title), las = 2)
points(sapply(names(movies[movies == 67]), function(m) mean(examples[examples[1] == m, ][, c(3)])), pch = 19, col = 'red')

dev.off()

make_user_feature <- function(n) function(u) {
    uvotes <- examples[examples$user == u,][,3]
    mean_vote <- sum(sapply(uvotes, function(v) {
        if (v == n) return(1)
        else return(0)
    })) / length(uvotes)
    return(round(mean_vote * 100) / 100)
}


users$ONE   <- sapply(users[,1], make_user_feature(1))
users$TWO   <- sapply(users[,1], make_user_feature(2))
users$THREE <- sapply(users[,1], make_user_feature(3))
users$FOUR  <- sapply(users[,1], make_user_feature(4))
users$FIVE  <- sapply(users[,1], make_user_feature(5))

rownames(users) = users$user
clustered_users <- hclust(dist(users[,c(2,6:10)]), method = "average")
twenty_clusters <- cutree(clustered_users, 20)

print('Number of users in each cluster:')
print(table(twenty_clusters))
print('', quote = FALSE)

print('Average age of people in each cluster:')
print(sapply(
    1:20,
    function(c) mean(users[sapply(
        users[1],
        function(u) u %in% names(twenty_clusters[twenty_clusters == c])), ][, 2])))
print('', quote = FALSE)

print('Existence of duplicates in each cluster:')
print(sapply(1:20, function(c) {
    cusers <- users[sapply(
        users[1],
        function(u) u %in% names(twenty_clusters[twenty_clusters == c]))]
    return(length(cusers) != length(unique(cusers)))
}))
print('', quote = FALSE)

# hcu <- as.dendrogram(clustered_users)
# plot(hcu,  main = "Users",  xlab="", sub="", cex=.9)
