Glass_data <- read.csv(file.choose())
View(Glass_data)
table(Glass_data$Type)
str(Glass_data)
# Recode Type as a factor
Glass_data$Type <- factor(Glass_data$Type,levels=c("1","2","3","4","5","6","7"),
                          labels=c("b_win_flo_pro",
                                   "b_win_non_flo_pro",
                                   "veh_win_flo_pro",
                                   "veh_win_non_flo_pro",
                                   "containers","tableware","headlamps"))                      
View(Glass_data)
Glass_data <- Glass_data[,c(ncol(Glass_data),1:(ncol(Glass_data)-1))]
View(Glass_data)

# Table or proportions with more informative labels
round(prop.table(table(Glass_data$Type))*100,digits=1)
## Summary of any three variables
summary(Glass_data[c("Na","Mg","Si")])
## Normalization Function ##
normalize <- function(x){
    return((x-min(x))/(max(x)-min(x))) 
}

## Applying normalization function on Glass Data ##
Glass_norm <- as.data.frame(lapply(Glass_data[,2:10],normalize)) 
View(Glass_norm)                            

## Create Training and Testing Dataset
Glass_train <- Glass_norm[1:170,]
View(Glass_train)
Glass_test <- Glass_norm[171:214,]
View(Glass_test)

## Creating Labels for training and testing data
Glass_train_Label <- Glass_data[1:170,1]
Glass_train_Label <- Glass_train_Label[["Type"]]
Glass_test_Label <- Glass_data[171:214,1]
Glass_test_Label <- Glass_test_Label[["Type"]]

## Training a Model on Data
library(class)
Glass_Model <- knn(train=Glass_train,test=Glass_test,cl=Glass_train_Label,k=9)
Glass_Model
library(gmodels)
CrossTable(Glass_test_Label,Glass_Model,prop.chisq = FALSE)
plot(Glass_Model,col=rainbow(7),main="Classification of Glass",
     xlab="Types of Glass")
## k=6 ##
Glass_Model <- knn(train=Glass_train,test=Glass_test,cl=Glass_train_Label,k=5)
CrossTable(Glass_test_Label,Glass_Model,prop.chisq = FALSE)
plot(Glass_Model,col=rainbow(7),main="Classification of Glass",
     xlab="Types of Glass")
## k=9 ##
Glass_Model <- knn(train=Glass_train,test=Glass_test,cl=Glass_train_Label,k=10)
CrossTable(Glass_test_Label,Glass_Model,prop.chisq = FALSE)
plot(Glass_Model,col=rainbow(7),main="Classification of Glass",
     xlab="Types of Glass")
## k=15 ##
Glass_Model <- knn(train=Glass_train,test=Glass_test,cl=Glass_train_Label,k=12)
CrossTable(Glass_test_Label,Glass_Model,prop.chisq = FALSE)
plot(Glass_Model,col=rainbow(7),main="Classification of Glass",
     xlab="Types of Glass")
## k=20 ##
Glass_Model <- knn(train=Glass_train,test=Glass_test,cl=Glass_train_Label,k=15)
CrossTable(Glass_test_Label,Glass_Model,prop.chisq = FALSE)
plot(Glass_Model,col=rainbow(7),main="Classification of Glass",
     xlab="Types of Glass")
