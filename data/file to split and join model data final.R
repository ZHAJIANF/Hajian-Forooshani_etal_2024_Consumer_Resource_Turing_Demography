setwd("/users/cx21atek/desktop")
data <- read.csv("Netlogo_consumer_resource_model_output.csv",header=F)
dim(data)
## break up the file into six different files becuase its too large to upload to github in a single file 
step.size <- dim(data)[1]/12
step.size

start.index <- 1
end.index <- step.size
for( file.index in 0:11){

	start.index <- 1 + file.index*step.size
	end.index <- step.size*(file.index+1)

	# break apart the data 
	data.chunk.for.file <- data[start.index:end.index, ] 
	# export the file 
	write.csv(data.chunk.for.file,paste(file.index, "Netlogo_consumer_resource_model_output.csv"),row.names=F)

	cat(start.index, end.index,"\n" )
}



##### now put the data together. 
file.index <- 0 
building.df <- read.csv(paste(file.index, "Netlogo_consumer_resource_model_output.csv"))
for(file.index in 1:12){
	#file.index <- 1
	single.df <- read.csv(paste(file.index, "Netlogo_consumer_resource_model_output.csv"))
	
	building.df <- rbind(building.df, single.df)
}
head(building.df)
dim(building.df)





