#45126125
#Assignment: Task 1


#Setting seed
set.seed(6125)

breastcancerdata2 <- breastcancerdata1

#Remove class 
to.remove <- "Class"
breastcancerdata2 <- breastcancerdata1[, !names(breastcancerdata1) %in% to.remove, drop = F]

names(breastcancerdata2)
#kmeans cluster the data
km = kmeans(breastcancerdata2,2)

#plot by default 
#save plot obtained in task 2.2
jpeg(filename = "./plots/Task_2.2.jpeg")
plot(breastcancerdata2[c("Clump.Thickness","Uniformity.of.Cell.Size")],col=km$cluster)
dev.off()

#plot by colour with 'class' column
#save plot obtained in task 2.3
jpeg(filename = "./plots/Task_2.3.jpeg")
plot(breastcancerdata2[c("Clump.Thickness","Uniformity.of.Cell.Size")],col=breastcancerdata1$Class)
dev.off()

#kmeans cluster the data, k=3
km3 = kmeans(breastcancerdata2,3)

#plot by default 
#save plot obtained in task 2.5, k=3
jpeg(filename = "./plots/Task_2.5_kequals3.jpeg")
plot(breastcancerdata2[c("Clump.Thickness","Uniformity.of.Cell.Size")],col=km3$cluster)
dev.off()

#kmeans cluster the data, k=4
km4 = kmeans(breastcancerdata2,4)

#plot by default 
#save plot obtained in task 2.5, k=4
jpeg(filename = "./plots/Task_2.5_kequals4.jpeg")
plot(breastcancerdata2[c("Clump.Thickness","Uniformity.of.Cell.Size")],col=km4$cluster)
dev.off()

#kmeans cluster the data, k=5
km5 = kmeans(breastcancerdata2,5)

#plot by default 
#save plot obtained in task 2.5, k=5
jpeg(filename = "./plots/Task_2.5_kequals5.jpeg")
plot(breastcancerdata2[c("Clump.Thickness","Uniformity.of.Cell.Size")],col=km5$cluster)
dev.off()

#sum of squares of each cluster
km$betweenss
km3$betweenss
km4$betweenss
km5$betweenss

#Apply hierarchical clustering to the data using the hclust function with default parameters
n=nrow(breastcancerdata2)
idx<-sample(1:n, 40)
breastcancerdata2sample <- breastcancerdata2[idx,]
h <- hclust(dist(breastcancerdata2sample))

jpeg(filename = "./plots/Task_2.7_cutsnone.jpeg", width= 1500, height= 1300)
plot(h,hang=-1,labels = breastcancerdata1$Class[idx])
dev.off()

#Cut the dendogram into 2 clusters
jpeg(filename = "./plots/Task_2.7_2cuts.jpeg", width= 1500, height= 1300)
plot(h,hang=-1,labels = breastcancerdata1$Class[idx])
nclust=2
rect.hclust(h,k=nclust)
groups2<-cutree(h, k=nclust)
dev.off()

#Cut the dendogram into 3 clusters
jpeg(filename = "./plots/Task_2.7_3cuts.jpeg", width= 1500, height= 1300)
plot(h,hang=-1,labels = breastcancerdata1$Class[idx])
nclust=3
rect.hclust(h,k=nclust)
groups3<-cutree(h, k=nclust)
dev.off()

#Cut the dendogram into 4 clusters
jpeg(filename = "./plots/Task_2.7_4cuts.jpeg", width= 1500, height= 1300)
plot(h,hang=-1,labels = breastcancerdata1$Class[idx])
nclust=4
rect.hclust(h,k=nclust)
groups4<-cutree(h, k=nclust)
dev.off()

#Cut the dendogram into 5
jpeg(filename = "./plots/Task_2.7_5cuts.jpeg", width= 1500, height= 1300)
plot(h,hang=-1,labels = breastcancerdata1$Class[idx])
nclust=5
rect.hclust(h,k=nclust)
groups5<-cutree(h, k=nclust)
dev.off()

#single
hs <- hclust(dist(breastcancerdata2sample), method= "single")

jpeg(filename = "./plots/Task_2.7_single.jpeg", width= 1500, height= 1300)
plot(hs,hang=-1,labels = breastcancerdata1$Class[idx])
dev.off()

#complete
hc <- hclust(dist(breastcancerdata2sample), method="complete")

jpeg(filename = "./plots/Task_2.7_complete.jpeg", width= 1500, height= 1300)
plot(hc,hang=-1,labels = breastcancerdata1$Class[idx])
dev.off()

#average
ha <- hclust(dist(breastcancerdata2sample), method="average")

jpeg(filename = "./plots/Task_2.7_average.jpeg", width= 1500, height= 1300)
plot(ha,hang=-1,labels = breastcancerdata1$Class[idx])
dev.off()










