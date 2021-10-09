car = read.table('C:/Users/25001/Desktop/R_DATA/car.data', sep = ',')
colnames(car) = c('buy','main','doors','capacity','lug_boot','safety','accept')
library(caret)
ind = createDataPartition(car$accept, times = 1,p=0.75,list=FALSE)
carTR = car[ind,]
carTE = car[-ind,]
library(randomForest)
randomForest.model = randomForest(accept~., data=carTR)


