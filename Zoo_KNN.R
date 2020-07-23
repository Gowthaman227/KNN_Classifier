Zoo_data <- read.csv(file.choose())
View(Zoo_data)
table(Zoo_data$type)
str(Zoo_data)
# Recode Type as a factor
Zoo_data$type <- factor(Zoo_data$type,levels=c("1","2","3","4","5","6","7"),
                        labels=c("Mammal","Bird","Reptile","Fish","Amphibian","Bug","Invertebra"))

Zoo_data <- Zoo_data[-1]
View(Zoo_data)
Zoo_data <- Zoo_data[,c(ncol(Zoo_data),1:(ncol(Zoo_data)-1))]
View(Zoo_data)

# Table or proportions with more informative labels
round(prop.table(table(Zoo_data$type)) * 100, digits = 1)

# Summarize any three features
summary(Zoo_data[c("feathers","eggs","milk")])

# create normalization function
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}        


## Normalize the Zoo Data
Zoo_norm <- as.data.frame(lapply(Zoo_data[2:17],normalize))
View(Zoo_norm)
summary(Zoo_norm$milk)

## Create Train and Test dataset
Zoo_train <- (Zoo_norm[1:70,])
View(is.na(Zoo_train))
View(Zoo_train)
dim(Zoo_train)
Zoo_test <- (Zoo_norm[71:101,])
View(is.na(Zoo_test))
View(Zoo_test)
dim(Zoo_test)

## Create Labels for Test and Train
Zoo_train_label <- Zoo_data[1:70, 1]
View(Zoo_train_label)
Zoo_train_label <- Zoo_train_label[["type"]]
Zoo_train_label
Zoo_test_label <- Zoo_data[71:101, 1]
Zoo_test_label <- Zoo_test_label[["type"]]
Zoo_train_label
## Training a Model on Data
library(class)
Zoo_Model <- knn(train=Zoo_train,test=Zoo_test,cl=Zoo_train_label,k=9)
Zoo_Model
library(gmodels)
## Creating Cross Table for predicted and actual values
CrossTable(x = Zoo_test_label, y = Zoo_Model,prop.chisq=FALSE)
plot(Zoo_Model, col = rainbow(7),main="Classification of Animlas",
     xlab="Types of Animals")

## K=6 ##
Zoo_Model <- knn(train=Zoo_train,test=Zoo_test,cl=Zoo_train_label,k=6)
CrossTable(x = Zoo_test_label, y = Zoo_Model,prop.chisq=FALSE)
plot(Zoo_Model, col = rainbow(7),main="Classification of Animlas",
     xlab="Types of Animals")
## K=7 ##
Zoo_Model <- knn(train=Zoo_train,test=Zoo_test,cl=Zoo_train_label,k=7)
CrossTable(x = Zoo_test_label, y = Zoo_Model,prop.chisq=FALSE)
plot(Zoo_Model, col = rainbow(7),main="Classification of Animlas",
     xlab="Types of Animals")
## K=8 ##
Zoo_Model <- knn(train=Zoo_train,test=Zoo_test,cl=Zoo_train_label,k=8)
CrossTable(x = Zoo_test_label, y = Zoo_Model,prop.chisq=FALSE)
plot(Zoo_Model, col = rainbow(7),main="Classification of Animlas",
     xlab="Types of Animals")
## K=10 ##
Zoo_Model <- knn(train=Zoo_train,test=Zoo_test,cl=Zoo_train_label,k=10)
CrossTable(x = Zoo_test_label, y = Zoo_Model,prop.chisq=FALSE)
plot(Zoo_Model, col = rainbow(7),main="Classification of Animlas",
     xlab="Types of Animals")
