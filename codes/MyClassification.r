#45126125
#Assignment: Task 1


breastcancerdata3<-breastcancerdata1

#setting seed
set.seed(6125)

#Divide the dataset into “training” and “test” subsets randomly (70% and 30% respectively)
m=nrow(breastcancerdata3)
training_precentage=0.7
test_percentage=0.3

#Sample random index
ind <- sample(2, m, replace = TRUE, prob = c(training_precentage, test_percentage))

#Set training and test data
training_data= breastcancerdata3[ind == 1, ]
test_data= breastcancerdata3[ind == 2, ]

training_data
test_data
breastcancerdata3

#Divide features and labels
training_features<-training_data[,1:9]
training_labels<-training_data[,10] 
test_features<-test_data[,1:9]
test_labels<-test_data[,10]

#install and import "party" library
install.packages("party")
library(party)

#Specify target class and predictors
myFormula<-Class ~ Clump.Thickness + Uniformity.of.Cell.Size + Uniformity.of.Cell.Shape + Marginal.Adhesion + Single.Epithelial.Cell.Size + Bare.Nuclei + Bland.Chromatin + Normal.Nucleoli + Mitoses

#generate classification tree
bcd_ctree <- ctree(myFormula, data = training_data)

#Visualize the tree
jpeg(filename = "./plots/Task_3.2.jpeg")
plot(bcd_ctree)
dev.off()
plot(bcd_ctree, type="simple")

#Predict class labels
ctree_pred <- predict(bcd_ctree, newdata = test_features)

#Calculate the accuracy, precision, and recall

#Create confusion matrix
cm=as.matrix(table(Actual=test_labels, Predicted=ctree_pred))

n=sum(cm) #no of instances
nc=nrow(cm) #no of classes
diag=diag(cm) #no of correctly classified instances per class
rowsums=apply(cm, 1, sum) #no of instances per class
colsums=apply(cm, 2, sum) #no of predictions per class

#compute accuracy, precision, recall and f1
accuracy=sum(diag)/n
precision=diag/colsums
recall=diag/rowsums
f1=2*precision*recall/(precision+recall)

results<-data.frame(precision,recall,f1)
accuracy
results
cm

#task3.3

bcd_ctree_modified<- ctree(myFormula, data = training_data, controls = ctree_control(teststat = c("quad", "max"), 
                                                                                     testtype = c("Bonferroni", "MonteCarlo", 
                                                                                                  "Univariate", "Teststatistic"), 
                                                                                   mincriterion = 0.95, minsplit = 20, minbucket = 7, 
                                                                                     stump = FALSE, nresample = 9999, maxsurrogate = 0,                                                                                     mtry = 0, savesplitstats = TRUE, maxdepth = 0, remove_weights = FALSE))
plot(bcd_ctree_modified)
plot(bcd_ctree)

#install and import "class" library
installed.packages("class")
library(class)

#Classifying using K-NN when k=1
knn_pred <- knn(training_features, test = test_features, cl=training_labels, k=1)

#Create confusion matrix
cm1 = as.matrix(table(Actual = test_labels, Predicted = knn_pred))

n1 = sum(cm1) #no of instances
nc1 = nrow(cm1) #no of classes
diag1 = diag(cm1) #no of correctly classified instances per class
rowsums1 = apply(cm1, 1, sum) #no of instances per class
colsums1 = apply(cm1, 2, sum) #no of predictions per class

#compute accuracy, precision, recall and f1
accuracy1 = sum(diag1) / n1
precision1 = diag1 / colsums1
recall1 = diag1 / rowsums1
f1_kequals1 = 2*precision1*recall1 / (precision1 + recall1)

results1 <- data.frame(precision1, recall1, f1_kequals1)
accuracy1
results1
cm1

#Classifying using K-NN when k=2
knn_pred2 <- knn(training_features, test = test_features, cl=training_labels, k=2)

#Create confusion matrix
cm2 = as.matrix(table(Actual = test_labels, Predicted = knn_pred2))

n2 = sum(cm2) #no of instances
nc2 = nrow(cm2) #no of classes
diag2 = diag(cm2) #no of correctly classified instances per class
rowsums2 = apply(cm2, 1, sum) #no of instances per class
colsums2 = apply(cm2, 2, sum) #no of predictions per class

#compute accuracy, precision, recall and f1
accuracy2 = sum(diag2) / n2
precision2=diag2 / colsums2
recall2 = diag2 / rowsums2
f1_kequals2 = 2*precision2*recall2 / (precision2+recall2)
results2 <- data.frame(precision2,recall2,f1_kequals2)



#Classifying using K-NN when k=3


knn_pred3 <- knn(training_features, test = test_features, cl=training_labels, k=3)

#Create confusion matrix
cm3 = as.matrix(table(Actual=test_labels, Predicted=knn_pred3))

n3 = sum(cm3) #no of instances
nc3 = nrow(cm3) #no of classes
diag3 = diag(cm3) #no of correctly classified instances per class
rowsums3 = apply(cm3, 1, sum) #no of instances per class
colsums3 = apply(cm3, 2, sum) #no of predictions per class

#compute accuracy, precision, recall and f1
accuracy3 = sum(diag3)/n3
precision3 = diag3/colsums3
recall3 = diag3 / rowsums3
f1_kequals3 = 2 * precision3 * recall3 / (precision3 + recall3)
results3 <- data.frame(precision3, recall3, f1_kequals3)


#Classifying using K-NN when k=4


knn_pred4<-knn(training_features, test = test_features, cl=training_labels, k=4)

#Create confusion matrix
cm4=as.matrix(table(Actual=test_labels, Predicted=knn_pred4))

n4=sum(cm4) #no of instances
nc4=nrow(cm4) #no of classes
diag4=diag(cm4) #no of correctly classified instances per class
rowsums4=apply(cm4, 1, sum) #no of instances per class
colsums4=apply(cm4, 2, sum) #no of predictions per class

#compute accuracy, precision, recall and f1
accuracy4 = sum(diag4) / n4
precision4 = diag4 / colsums4
recall4 = diag4 / rowsums4
f1_kequals4 = 2*precision4*recall4 / (precision4+recall4)
results4 <- data.frame(precision4,recall4,f1_kequals4)


#Classifying using K-NN when k=5


knn_pred5<-knn(training_features, test = test_features, cl=training_labels, k=5)

#Create confusion matrix
cm5=as.matrix(table(Actual=test_labels, Predicted=knn_pred5))

n5=sum(cm5) #no of instances
nc5=nrow(cm5) #no of classes
diag5=diag(cm5) #no of correctly classified instances per class
rowsums5=apply(cm5, 1, sum) #no of instances per class
colsums5=apply(cm5, 2, sum) #no of predictions per class

#compute accuracy, precision, recall and f1
accuracy5 = sum(diag5) / n5
precision5 = diag5 / colsums5
recall5 = diag5 / rowsums5
f1_kequals5 = 2*precision5*recall5 / (precision5+recall5)
results5 <- data.frame(precision5,recall5,f1_kequals5)

#Printing data for comparison
accuracy1
results1
cm1

accuracy2
results2
cm2

accuracy3
results3
cm3

accuracy4
results4
cm4

accuracy5
results5
cm5


