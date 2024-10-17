
rm(list=ls())
data <- read.csv("Azteca_census_data_Jan_2018.csv")
head(data)
library(igraph)


age_matrix <- matrix(NA, ncol=13, nrow=13)
#how many died divided by how many were there
columns_to_sample <- seq(14,26,1)
head(data[, columns_to_sample]) ## double check here 

for(year.looper in 1:13){ ## this loops through columns starting in 2004. ending in 2016
	#year.looper <- 1
	a_years_data <- data[,	columns_to_sample[year.looper]]
	age_freq_dist <- numeric(length(columns_to_sample))
	for(age.looper in 1:length(age_freq_dist)){
		#age.looper <- 1
		age_freq_dist[age.looper] <-  sum(a_years_data == age.looper)
	}
	
	age_matrix[,year.looper] <-age_freq_dist
}

colnames(age_matrix) <- c("2004","2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")
rownames(age_matrix) <- c("1yr","2yr","3yr","4yr","5yr","6yr","7yr","8yr","9yr","10yr","11yr","12yr","13yr")


# Now we want to make a matrix with the age specific mortality between each year
# So (age2_t2-age1_t1)/age1_t1 
# How many one year old nests made it to two years old divided by the inital number of one year old nests 
#### We have 13 years of data so we can calculate 12 transitions 

mortality_matrix <- matrix(NA,ncol=12,nrow=12)
for(year.looper in 1:12){ ## looping through the times 
	#year.looper <- 1 # testing
	age_mortality_tras_vec <-numeric(12)
	for(age.looper in 1:12){ ## looping through the ages 
		#age.looper <- 1 # testings
		this_year_age <-  age_matrix[age.looper, year.looper]
		next_year_age_plus_one <- age_matrix[age.looper+1, year.looper+1] 
		
		## calculate the fraction that surives to the next year 
		
		transition.mortality <- (next_year_age_plus_one - this_year_age)/this_year_age
		age_mortality_tras_vec[age.looper] <- transition.mortality
		
		
	}
		mortality_matrix[, year.looper] <- age_mortality_tras_vec

}

colnames(mortality_matrix) <- c("2005","2006","2007","2008","2009","2010","2011","2012","2013","2014","2015","2016")
rownames(mortality_matrix) <- c("1yr","2yr","3yr","4yr","5yr","6yr","7yr","8yr","9yr","10yr","11yr","12yr")

mortality_matrix ## this give the mortality rates for the following year. So 2005 shows the mortality rate for the age classes of 2004

### Here I am looking at individual years ageclass mortality
year_vec<- colnames(mortality_matrix)[3:length(colnames(mortality_matrix))]
par(mfrow=c(2,5))
fitting_estimate <-c()
all_estimate <-c()
all_r <-c()
fitting_r <-c()
for(x in 1:10){
	
	#x <- 1
	a_years_mortality_raw <- t(mortality_matrix[, year_vec[x]])
	a_years_age_raw <- t(mortality_matrix[,year_vec[x]])

	a_year_mortality_clean <- a_years_mortality_raw[!is.nan(a_years_mortality_raw)]
	
	## logged response
	#a_year_age_clean <- log(seq(1,length(a_year_mortality_clean),1))
	#a_year_mortality_clean <- log(abs(a_year_mortality_clean))


	## raw response
	a_year_age_clean <-(seq(1,length(a_year_mortality_clean),1))
	a_year_mortality_clean <-((a_year_mortality_clean))


	
	age_fitting <- a_year_age_clean[1:5]
	death_fitting <- a_year_mortality_clean[1:5]


## logged plots 
	#plot(a_year_age_clean, a_year_mortality_clean,col="grey" ,pch=19,xlab="",ylab="",yaxt="n",ylim=c(-2.6,0))
	#axis(2,las=2)
	#mtext("ln(Nest age class)",line=2.2,side=1,cex=0.7)
	#mtext("ln(Death rate)",line=2.7,side=2,cex=0.7)
	
	
	plot(a_year_age_clean, a_year_mortality_clean,col="grey" ,pch=19,xlab="",ylab="",yaxt="n",ylim=c(-0.5,0))
	axis(2,las=2)
	mtext("Nest age class",line=2.2,side=1,cex=0.7)
	mtext("Death rate",line=2.7,side=2,cex=0.7)

	points(age_fitting, death_fitting,pch=19 )

## Looking at the fit to just years 1:5 The hypothesis is that the effect of the DSP on mortality is mainly in those years.
	model_fitting <- lm(death_fitting~ age_fitting)
	fitting_estimate <-c(fitting_estimate, summary(model_fitting)$coefficients[2,1])

## here fitting all the data
	abline(model_fitting,lwd=2,col="black")
	model <- lm(a_year_mortality_clean~ a_year_age_clean)
	abline(model,col="grey",lwd=2)
	all_estimate <-c(all_estimate,summary(model)$coefficients[2,1] )

	mtext(year_vec[x])
	all_r <-c(all_r,summary(model)$r.squared)
	fitting_r <-c(fitting_r, summary(model_fitting)$r.squared)

}


par(mfrow=c(1,2))
index <- seq(2007,2016,1)
plot(index ,fitting_estimate,yaxt="n",pch=19,xlab="year",ylab="estimate for log-log fit",ylim=c(0,0.15))
axis(2,las=2)
points(index ,all_estimate,pch=17,col="grey")

index <- seq(2007,2016,1)
plot(index , fitting_r,yaxt="n",pch=19,xlab="year",ylab="R squared for log-log fit",ylim=c(0,1))
axis(2,las=2)
points(index , all_r,pch=17,col="grey")


### Here I can get the means from the mortality to look at ageclass mean
age_mortality_mat <- t(mortality_matrix) ## columns are ages and rows are between year mortality rates for each age calss


## putting data in long format Nov 6 2023

dim(age_mortality_mat)## using 12 years of data ## each column is a year


### Now looking at the age specific mortality but using all of the data avaible, not just the means 
df.raw <- data.frame(0,0)
colnames(df.raw) <- c("age","death.rate")
for(x in 1:12){
	#x <- 1
	temp.df <- data.frame(x,age_mortality_mat[,x])
	colnames(temp.df) <- c("age","death.rate")
	df.raw <- rbind(df.raw, temp.df)
}

death.rate.df <- df.raw[df.raw$age != 0 &  !is.nan(df.raw$death.rate),]
death.rate.df$abs.death <- abs(death.rate.df$death.rate)

#write.csv(death.rate.df,"2023_11_15_Azteca_DSP_empirical_death_rates_full_df.csv",row.names=F)
