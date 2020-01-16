#45126125
#Assignment: Task 1


#Extract Data
breastcancerdata<-read.table("./data/breast-cancer-wisconsin.data", sep = ',')

#Assign names to variables.
names(breastcancerdata)<- c("sample code number","Clump Thickness","Uniformity of Cell Size","Uniformity of Cell Shape","Marginal Adhesion","Single Epithelial Cell Size","Bare Nuclei","Bland Chromatin","Normal Nucleoli","Mitoses","Class")

#printing data
breastcancerdata

#Substitute space with dot
names(breastcancerdata)<-gsub(" ",".",names(breastcancerdata))
names(breastcancerdata)

str(breastcancerdata)

breastcancerdata[1:100, "Bare.Nuclei"]
breastcancerdata$Bare.Nuclei[100:300]

#Removing rows with "?" in Bare.Nuclei from Dataset
breastcancerdata1<-breastcancerdata[!breastcancerdata$Bare.Nuclei == "?",]

str(breastcancerdata1)

breastcancerdata1[breastcancerdata1$Bare.Nuclei == "?", ]

#Deleting "?" from Bare.Nuclei level
breastcancerdata1$Bare.Nuclei <- factor(breastcancerdata1$Bare.Nuclei)

#Changing column type from factor to Integer
breastcancerdata1$Bare.Nuclei <- as.integer(as.character(breastcancerdata1$Bare.Nuclei))

#Removing Sample code number
to.delete <- "sample.code.number"
breastcancerdata1 <- breastcancerdata1[, !names(breastcancerdata1) %in% to.delete, drop = F]

#Notice that R might define the “class” column as integer. In that case, change its type from integer to factor
levels(breastcancerdata1$Class)
breastcancerdata1$Class <- factor(breastcancerdata1$Class)

saveRDS(breastcancerdata1,file = "./data//bcw_processed.Rda")

bcw<-readRDS("./data//bcw_processed.Rda")
bcw
