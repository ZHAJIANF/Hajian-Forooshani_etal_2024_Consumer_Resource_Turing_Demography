
data <- read.csv("Netlogo_consumer_resource_model_output",header=T,skip=6)
data$replicate <- data$X.run.number.
data$sim.step <- data$X.step.
pred.diff.val.vec <- unique(data$normal_mean_walk)

## what is the maximum size cluster we can detect? 
max.cluster.size.to.use <- 10000


clust.df.colnames <- c("pred.diff","rep.id","cluster.size","cum.freq")

clust.freq.df.raw <- data.frame(matrix(0, ncol=length(clust.df.colnames)))
colnames(clust.freq.df.raw) <- clust.df.colnames

for(walk.val.looper in 1:length(pred.diff.val.vec)){ ## select a diffusion parameter 
	
	#walk.val.looper <- 2
	a.walk.df <- data[data$normal_mean_walk == pred.diff.val.vec[walk.val.looper], ]
	
	head(a.walk.df)
	rep.counter.indexy <- 1
	for(replicate.looper in unique(a.walk.df$replicate)){### going through the unique replicates of the simluations 
		
		
	#replicate.looper <- 17
	a.replicate.walk.df <- a.walk.df[a.walk.df$replicate == replicate.looper,] 

	
	raw_cluster <- as.character(a.replicate.walk.df$cluster.freq[1001])
	split_cluster <- unlist(strsplit(raw_cluster, split = " "))
	first <- split_cluster[1]
	last <- split_cluster[length(split_cluster)]
	split_cluster[1] <- gsub("\\[","",first)
	split_cluster[length(split_cluster)] <- gsub("\\]","",last)
	split_cluster <- as.numeric(split_cluster)
	split_cluster <- rev(sort(split_cluster))
	## the empty space is counted as a cluster in netlogo. so it has to be removed. It is always the biggest cluster 
	clusters_minus_biggest <- split_cluster[split_cluster != max(split_cluster)]
	cluster_sizes <- clusters_minus_biggest
	


	cluster_freq <- numeric(max.cluster.size.to.use)
	for( x in 1: max.cluster.size.to.use){
		cluster_freq[x] <- sum(cluster_sizes == x)
	}
	
	cumm_freq_dist <-numeric(length(cluster_freq))
	cumm_freq_dist[length(cumm_freq_dist)] <- cluster_freq[length(cumm_freq_dist)]
	for(dingers in (length(cumm_freq_dist)-1):1){
		cumm_freq_dist[dingers] <- cumm_freq_dist[dingers + 1] + cluster_freq[dingers]
	}
	
	index <- seq(1,length(cumm_freq_dist),1)
	
	if(sum(log(cumm_freq_dist) > 0)){
		
			plot(log(index),log(cumm_freq_dist))

	}

	temp.df.storage <- data.frame(pred.diff.val.vec[walk.val.looper], rep.counter.indexy ,index, cumm_freq_dist)
	colnames(temp.df.storage) <- clust.df.colnames
	
	clust.freq.df.raw <- rbind(clust.freq.df.raw, temp.df.storage)
	rep.counter.indexy <- rep.counter.indexy  + 1
	}# end of replicate.looper
	
}# end of walk.val.looper


## the bunch of zeros is fucking up the models. Since it is cummulative dsitributions we shoudl only be interested in non-zero y values
### we can the nremove the zeros from the cum.freq
clust.freq.df <- clust.freq.df.raw[clust.freq.df.raw$rep.id != 0 & clust.freq.df.raw$cum.freq > 0 , ]

head(clust.freq.df)

dif.clust.df<-clust.freq.df[clust.freq.df$pred.diff == 0.5, ]
plot(log(dif.clust.df$cluster.size), log(dif.clust.df$cum.freq),col=viridis(10)[dif.clust.df$rep.id],xlim=c(0,9),ylim=c(0,5.5),cex=0.5)
full.linear.model <- lm(log(dif.clust.df$cum.freq) ~ log(dif.clust.df$cluster.size) )
summary(full.linear.model)


dif.clust.df<-clust.freq.df[clust.freq.df$pred.diff == 1.5, ]
plot(log(dif.clust.df$cluster.size), log(dif.clust.df$cum.freq),col=viridis(10)[dif.clust.df$rep.id],xlim=c(0,9),ylim=c(0,5.5),cex=0.5)
full.linear.model <- lm(log(dif.clust.df$cum.freq) ~ log(dif.clust.df$cluster.size) )
summary(full.linear.model)

dif.clust.df<-clust.freq.df[clust.freq.df$pred.diff == 2.5, ]
plot(log(dif.clust.df$cluster.size), log(dif.clust.df$cum.freq),col=viridis(10)[dif.clust.df$rep.id],xlim=c(0,9),ylim=c(0,5.5),cex=0.5)
full.linear.model <- lm(log(dif.clust.df$cum.freq) ~ log(dif.clust.df$cluster.size) )
summary(full.linear.model)







setwd("/users/cx21atek/desktop/Azteca_DSP_local/input data")
dir()
data <- read.csv("Azteca_census_data_Jan_2018.csv")
library(igraph)

nests_2016 <- data[data$y16 > 0, ]
nests_2016_DM <- as.matrix(dist(data.frame(as.numeric(nests_2016$x_coord),as.numeric(nests_2016$y_coord) )))
head(nests_2016_DM)


#clust.scale.stat.looper <- 1
Cluster_scale <- 20 #cluster.scale.vec[clust.scale.stat.looper] #dingus_smingus
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

nests_2016$cluster_ID <- groups
nest.df <- nests_2016 ## transfer to new data frame name 

cluster_sizes <-numeric(max(nest.df$cluster_ID))
for(zip in 1:max(nest.df $cluster_ID)){
	cluster_sizes[zip] <-  sum(nest.df $cluster_ID == zip)
}

## frequency of the cluste rsizes 
freq_dist <-max(cluster_sizes)
for(dip in 1:max(cluster_sizes)){
	freq_dist[dip] <- sum(cluster_sizes == dip) 
}

cum_freq_dist <-numeric(length(freq_dist))
cum_freq_dist[length(cum_freq_dist)] <- freq_dist[length(freq_dist)]
for(eip in (length(cum_freq_dist)-1):1){
	cum_freq_dist[eip] <- cum_freq_dist[eip + 1] + freq_dist[eip]
}

#par(mfrow=c(1,1))
index <- seq(1,length(cum_freq_dist),1)
x <- index
x_log <- log(index)
y_log <- log(cum_freq_dist)
plot(x_log, y_log,yaxt="n",xlab="",ylab="",pch=1,lwd=1,xlim=c(0,8),ylim=c(0,8),cex=0.5)
#mtext("ln(cluster size)",side=1,line=2)
#mtext("ln(frequency)",side=2,line=2.0)
axis(2,las=2)
power_model <- lm(y_log ~ x_log)
abline(power_model,lwd=1)
summary(power_model)

empirical.x_log <- x_log
empirical.y_log <- y_log


pred.diff.vals <- unique(clust.freq.df$pred.diff)
dev.new(width=10,height=4)
par(mfrow=c(2,5),mai=c(0.3,0.3,0.1,0.1),oma=c(2,2,1,1))
for(pred.looper in 1:length(pred.diff.vals)){
	
	dif.clust.df<-clust.freq.df[clust.freq.df$pred.diff == pred.diff.vals[pred.looper], ]
	plot(log(dif.clust.df$cluster.size), log(dif.clust.df$cum.freq),col=turbo(10)[dif.clust.df$rep.id],xlim=c(0,9),ylim=c(0,5.5),cex=0.5,xlab="",ylab="",yaxt="n",lwd=0.7)
	#plot(log(dif.clust.df$cluster.size), log(dif.clust.df$cum.freq),col="grey",xlim=c(0,9),ylim=c(0,5.5),cex=0.5,xlab="",ylab="",yaxt="n")

	axis(2,las=2)

	full.linear.model <- lm(log(dif.clust.df$cum.freq) ~ log(dif.clust.df$cluster.size) )
	summary(full.linear.model)
	
	points(empirical.x_log, empirical.y_log,yaxt="n",xlab="",ylab="",pch=1,lwd=1,cex=1,type="l")

	points(empirical.x_log, empirical.y_log,yaxt="n",xlab="",ylab="",pch=19,lwd=1,cex=1)

	mtext(paste("consumer diffusion:",pred.diff.vals[pred.looper]),cex=0.7)
}
mtext("ln(cluster size)",side=1,line=0,outer=T)
mtext("ln(frequency)",side=2,line=0,outer=T)







