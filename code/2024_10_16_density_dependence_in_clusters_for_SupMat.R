rm(list = ls())
library(igraph)
data <- read.csv("Azteca_census_data_Jan_2018.csv")
head(data)

nests_2016 <- data[data$y16 > 0, ]
nests_2016_DM <- as.matrix(dist(data.frame(as.numeric(nests_2016$x_coord),as.numeric(nests_2016$y_coord) )))

Cluster_scale <-20
adj_matrix <- matrix(0,ncol=dim(nests_2016)[1],nrow=dim(nests_2016)[1])
for(i in 1:dim(nests_2016_DM)[1]){
	for( j in 1:dim(nests_2016_DM)[1]){
		if(nests_2016_DM[i,j] <= Cluster_scale & i != j ){
			adj_matrix[i,j] <- 1
		}else{
			adj_matrix[i,j] <- 0
			}
	}
}


nest_graph <- graph.adjacency(adj_matrix,mode="undirected")
groups <- unlist(clusters(nest_graph)[1])
V(nest_graph)$color <- groups
#E(nest_graph)$color <- groups


## Take some layout and then change the coordinates to match our real xy positions
zippin <-layout.fruchterman.reingold(nest_graph)
zippin[,1] <- nests_2016$y_coord
zippin[,2] <- nests_2016$x_coord

#plot(nest_graph,layout = zippin,vertex.size = 2, vertex.label = NA,edge.arrow.size = 0.01,edge.color="black",edge.width=1)#,vertex.color="black")

adj_ID_list <- order(groups)
#par(mfrow=c(1,1))
#image(adj_matrix[sort(groups), sort(groups)],col=c("white","black"),yaxt="n",xaxt="n")


## Now makesthe groups attached to the data set 
nests_2016$clusterID <-groups



#### now putting the ages and cluster size into the full data frame 


nest.df <- nests_2016
cluster.size <- numeric(max(nest.df$clusterID))
cluster.age <-numeric(max(nest.df$clusterID))
cluster.age.error <-numeric(max(nest.df$clusterID))
cluster.ID <-numeric(max(nest.df$clusterID)) 
old.nest.vec <-numeric(max(nest.df$clusterID)) 
young.nest.vec <-numeric(max(nest.df$clusterID)) 
for(x in 1:max(nest.df$clusterID)){
	#x <- 1
	a.clust.data <- nest.df[nest.df$clusterID == x, ]
	cluster.ID[x] <- unique(a.clust.data $clusterID)
	cluster.age[x] <- mean(a.clust.data$y16)
	cluster.age.error[x] <- sd(a.clust.data$y16)/sqrt( length(a.clust.data$y16))
	cluster.size[x] <- dim(a.clust.data)[1]
	
	if(sum(a.clust.data$y16 > 6) > 0 ){ ## if there is a cluster that is older than 6 years od 
		old.nest.vec[x] <- 1
	}else if(sum(a.clust.data$y16 <= 6) > 0 ){
		young.nest.vec[x] <- 1
	}
	
}
cluster.df <- data.frame(cluster.ID, cluster.age, cluster.age.error, cluster.size, old.nest.vec, young.nest.vec)

#par(mfrow=c(1,3), mai=c(0.6,0.6,0.2,0.2))

par(mfrow=c(2,3), mai=c(0.6,0.6,0.2,0.2))


plot(cluster.df$cluster.age,cluster.df$cluster.size,xlab="",ylab="",yaxt="n",pch=19)
arrows(cluster.df$cluster.age + cluster.age.error,cluster.df$cluster.size,cluster.df$cluster.age - cluster.age.error,cluster.df$cluster.size,angle=90,code=3,length=0)
mtext("Nest age in cluster",side=1,line=2.2)
mtext("# nests in cluster",side=2,line=2.2)
axis(2,las=2)


plot(cluster.df$cluster.age,cluster.df$old.nest.vec,xlab="",ylab="",yaxt="n",pch=19)
mtext("Nest age in cluster",side=1,line=2.3)
mtext("Old nest in cluster?",side=2,line=2.2)
axis(2,las=2)
abline(v=6,lty=3)
#plot(cluster.df$cluster.age,cluster.df$young.nest.vec,xlab="",ylab="",yaxt="n")
#mtext("mean nest age in cluster",side=1,line=2.2)
#mtext("Young nest in cluster?",side=2,line=2.2)
#axis(2,las=2)


old.clust.df <- cluster.df[cluster.df$old.nest.vec == 1,]
young.clust.df <- cluster.df[cluster.df$young.nest.vec == 1,]


old.val <- old.clust.df$cluster.size
young.val <- young.clust.df$cluster.size
means <- c(mean(young.val), mean(old.val) )
errors <- c(sd(young.val)/sqrt(length(young.val)),sd(old.val)/sqrt(length(old.val)) )


index <- seq(1,2,1)
plot(index,means,ylab="",xlab="",yaxt="n",xaxt="n",ylim=c(0,6),xlim=c(0.5,2.5),pch=19)
arrows(index,means+errors,index,means-errors,code=3,angle=90,length=0.03)
axis(2,las=2)
axis(1,labels=c("No","Yes"),at=index)
mtext("Cluster size",side=2,line=2.2)
mtext("Cluster contains old nests?",side=1,line=2.2)
mtext("12m spatial scale",side=4,cex=0.8,line=0.3)


### Now for 26 m 



Cluster_scale <-26 #dingus_smingus
adj_matrix <- matrix(0,ncol=dim(nests_2016)[1],nrow=dim(nests_2016)[1])
for(i in 1:dim(nests_2016_DM)[1]){
	for( j in 1:dim(nests_2016_DM)[1]){
		if(nests_2016_DM[i,j] <= Cluster_scale & i != j ){
			adj_matrix[i,j] <- 1
		}else{
			adj_matrix[i,j] <- 0
			}
	}
}


nest_graph <- graph.adjacency(adj_matrix,mode="undirected")
groups <- unlist(clusters(nest_graph)[1])
V(nest_graph)$color <- groups
#E(nest_graph)$color <- groups


## Take some layout and then change the coordinates to match our real xy positions
zippin <-layout.fruchterman.reingold(nest_graph)
zippin[,1] <- nests_2016$y_coord
zippin[,2] <- nests_2016$x_coord

#plot(nest_graph,layout = zippin,vertex.size = 2, vertex.label = NA,edge.arrow.size = 0.01,edge.color="black",edge.width=1)#,vertex.color="black")

adj_ID_list <- order(groups)
#par(mfrow=c(1,1))
#image(adj_matrix[sort(groups), sort(groups)],col=c("white","black"),yaxt="n",xaxt="n")


## Now makesthe groups attached to the data set 
nests_2016$clusterID <-groups



#### now putting the ages and cluster size into the full data frame 


nest.df <- nests_2016
cluster.size <- numeric(max(nest.df$clusterID))
cluster.age <-numeric(max(nest.df$clusterID))
cluster.age.error <-numeric(max(nest.df$clusterID))
cluster.ID <-numeric(max(nest.df$clusterID)) 
old.nest.vec <-numeric(max(nest.df$clusterID)) 
young.nest.vec <-numeric(max(nest.df$clusterID)) 
for(x in 1:max(nest.df$clusterID)){
	#x <- 1
	a.clust.data <- nest.df[nest.df$clusterID == x, ]
	cluster.ID[x] <- unique(a.clust.data $clusterID)
	cluster.age[x] <- mean(a.clust.data$y16)
	cluster.age.error[x] <- sd(a.clust.data$y16)/sqrt( length(a.clust.data$y16))
	cluster.size[x] <- dim(a.clust.data)[1]
	
	if(sum(a.clust.data$y16 > 6) > 0 ){ ## if there is a cluster that is older than 6 years od 
		old.nest.vec[x] <- 1
	}else if(sum(a.clust.data$y16 <= 6) > 0 ){
		young.nest.vec[x] <- 1
	}
	
}
cluster.df <- data.frame(cluster.ID, cluster.age, cluster.age.error, cluster.size, old.nest.vec, young.nest.vec)


#par(mfrow=c(1,3), mai=c(0.6,0.6,0.2,0.2))

plot(cluster.df$cluster.age,cluster.df$cluster.size,xlab="",ylab="",yaxt="n",pch=19)
arrows(cluster.df$cluster.age + cluster.age.error,cluster.df$cluster.size,cluster.df$cluster.age - cluster.age.error,cluster.df$cluster.size,angle=90,code=3,length=0)
mtext("Nest age in cluster",side=1,line=2.2)
mtext("# nests in cluster",side=2,line=2.2)
axis(2,las=2)


plot(cluster.df$cluster.age,cluster.df$old.nest.vec,xlab="",ylab="",yaxt="n",pch=19)
mtext("Nest age in cluster",side=1,line=2.2)
mtext("Old nest in cluster?",side=2,line=2.3)
axis(2,las=2)
abline(v=6,lty=3)


#plot(cluster.df$cluster.age,cluster.df$young.nest.vec,xlab="",ylab="",yaxt="n")
#mtext("mean nest age in cluster",side=1,line=2.2)
#mtext("Young nest in cluster?",side=2,line=2.2)
#axis(2,las=2)


old.clust.df <- cluster.df[cluster.df$old.nest.vec == 1,]
young.clust.df <- cluster.df[cluster.df$young.nest.vec == 1,]


old.val <- old.clust.df$cluster.size
young.val <- young.clust.df$cluster.size
means <- c(mean(young.val), mean(old.val) )
errors <- c(sd(young.val)/sqrt(length(young.val)),sd(old.val)/sqrt(length(old.val)) )

index <- seq(1,2,1)
plot(index,means,ylab="",xlab="",yaxt="n",xaxt="n",ylim=c(0,25),xlim=c(0.5,2.5),pch=19)
arrows(index,means+errors,index,means-errors,code=3,angle=90,length=0.03)
axis(2,las=2)
axis(1,labels=c("No","Yes"),at=index)
mtext("Cluster size",side=2,line=2.2)
mtext("Cluster contains old nests?",side=1,line=2.2)
mtext("26m spatial scale",cex=0.8,side=4,line=0.3)



