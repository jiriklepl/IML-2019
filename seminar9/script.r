library(e1071)
gram_rel<-read.table('fv.c.1.gram.rel.traindev.csv', header=T, sep='\t')

train.names<-readLines('train.txt')
dev.names<-readLines('dev.txt')

train<-gram_rel[gram_rel$file %in% train.names,]
dev<-gram_rel[gram_rel$file %in% dev.names,]

# let's train
model<-svm(class ~., train, kernel = "linear", cost = 1)
prediction<-predict(model, dev, type="class")
message(mean(prediction == dev$class)) # should be 0.3998909

print(table(prediction, dev$class))

model.poly<-svm(class ~., train, kernel = "polynomial", degree = 2, cost = 1)
prediction.poly<-predict(model.poly, dev, type="class")
message(mean(prediction.poly == dev$class)) # should be 0.3998909