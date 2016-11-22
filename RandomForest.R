rm(list=ls())
data <- read.table("~/desktop/data_filtered.csv",sep=',',header = T)
matrix = as.matrix(data)

length(which(colsum<(i+1)))-length(which(colsum<i))

length(which(data$biaoqian==1))
colsum = colSums(data)
colsum1 = array(0,6457)
colsum2 = array(0,6457)
colsum3 = array(0,6457)
colsum4 = array(0,6457)
colsum5 = array(0,6457)
for (i in 1:3505){
  if (matrix[i,6458]==1){colsum1 = colsum1 + matrix[i,-6458]}
  if (matrix[i,6458]==2){colsum2 = colsum2 + matrix[i,-6458]}
  if (matrix[i,6458]==3){colsum3 = colsum3 + matrix[i,-6458]}
  if (matrix[i,6458]==4){colsum4 = colsum4 + matrix[i,-6458]}
  if (matrix[i,6458]==5){colsum5 = colsum5 + matrix[i,-6458]}
}
COLsum = as.data.frame(rbind(colsum1,colsum2,colsum3,colsum4,colsum5,colsum))

once=which(colsum==1)
morethan_once=data[,-once]

twice=which(colsum<=2)
morethan_twice=data[,-twice]

library(randomForest)
set.seed(15)
morethan_twice$biaoqian = as.factor(morethan_twice$biaoqian)
attach(morethan_twice)
rf.data = randomForest(biaoqian~.-biaoqian,data=morethan_twice, mtry=80,ntree=2)
