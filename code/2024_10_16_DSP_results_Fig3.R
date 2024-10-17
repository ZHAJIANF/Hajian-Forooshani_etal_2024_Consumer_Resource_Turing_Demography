rm(list=ls())
## here are the DSP estimates from the empirical data 
DSP.summary.df <- read.csv("dsp.df.final.csv")
head(DSP.summary.df) 

## here are the DSP estimates from the randomization procedure 
RANDOMIZED.DSP.summary.df2 <- read.csv("dsp.randomizations.df.final.csv")

par(mfrow=c(1,3))
estimate.bootstrap.pval <- numeric(length(unique(DSP.summary.df$radius)))
Rsquare.bootstrap.pval  <- numeric(length(unique(DSP.summary.df$radius)))
pval.bootstrap.pval <- numeric(length(unique(DSP.summary.df$radius)))
dat.indexer <- 1
for(rad.looper in min(DSP.summary.df$radius):max(DSP.summary.df$radius)){ 
a.radii <- rad.looper
real.dsp.df <- DSP.summary.df[DSP.summary.df$radius == a.radii,]
rand.dsp.df <- RANDOMIZED.DSP.summary.df2[RANDOMIZED.DSP.summary.df2$radius == a.radii,]


## prob of finding a more negative DSP trend?
hist(rand.dsp.df $L.DSP.LE.estimates,breaks=100)
abline(v= real.dsp.df $L.DSP.LE.estimates)
estimate.pval <- sum(rand.dsp.df $L.DSP.LE.estimates <= real.dsp.df $L.DSP.LE.estimates)/length(rand.dsp.df $L.DSP.LE.estimates)

## prob of finding a bigger rsquared?
hist(rand.dsp.df $L.DSP.LE.rsquareds,breaks=100)
abline(v= real.dsp.df $L.DSP.LE.rsquareds)
rsquared.pval <- sum(rand.dsp.df $L.DSP.LE.rsquareds >= real.dsp.df $L.DSP.LE.rsquareds)/length(rand.dsp.df $L.DSP.LE.rsquareds)

## prob of finding a lower p value??
hist(rand.dsp.df $L.DSP.LE.pvals,breaks=100)
abline(v= real.dsp.df $L.DSP.LE.pvals)
pvals.pval <- sum(rand.dsp.df $L.DSP.LE.pvals <= real.dsp.df $L.DSP.LE.pvals)/length(rand.dsp.df $L.DSP.LE.pvals)


estimate.bootstrap.pval[dat.indexer] <- estimate.pval
Rsquare.bootstrap.pval[dat.indexer] <- rsquared.pval
pval.bootstrap.pval[dat.indexer] <- pvals.pval
dat.indexer <- dat.indexer + 1
}


plot(estimate.bootstrap.pval)
abline(h=0.05)
plot(Rsquare.bootstrap.pval)
abline(h=0.05)
plot(pval.bootstrap.pval)
abline(h=0.05)


radius.vals <- seq(10,50,1)
bs.rsquared.sig.pvals <- radius.vals[Rsquare.bootstrap.pval <= 0.01]
bs.pval.sig.pvals <- radius.vals[pval.bootstrap.pval <= 0.01]
bs.estimate.sig.pvals <- radius.vals[estimate.bootstrap.pval <= 0.01]

points(radius.vals, Rsquare.bootstrap.pval)




dev.new(height=3.2,width=9)
par(mfrow=c(1,3),mai=c(0.6,0.6,0.3,0.1))
label.cex <- 0.8
plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.estimates,ylim=c(-0.06,0.04),xlab="",ylab="",yaxt="n")
axis(2,las=2)
abline(h=0.0,lty=3)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.estimates,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.estimates,pch=19,type="l")
means <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.estimates,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.estimates,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")
mtext("spatial scale(m)",side=1,line=2.3,cex= label.cex)
mtext("DSP estimate",side=2,line=3,cex= label.cex)
#mtext("Linear DSP (linear pooled)",line=0.5,cex= label.cex)
points(bs.estimate.sig.pvals,rep(0.04,length(bs.estimate.sig.pvals)),pch="*",cex=1)
legend(35,0.035,legend=c("empirical","randomized"),col=c("black","red"),pch=19,bty="n")


plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.rsquareds,ylim=c(0,1),xlab="",ylab="",yaxt="n")
axis(2,las=2)
abline(h=0.0,lty=3)
means <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.rsquareds,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.rsquareds,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.rsquareds,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.rsquareds,pch=19,type="l")
mtext("spatial scale(m)",side=1,line=2.3,cex= label.cex)
mtext("DSP R-squared",side=2,line=3,cex= label.cex)
points(bs.rsquared.sig.pvals,rep(1,length(bs.rsquared.sig.pvals)),pch="*",cex=1)


plot(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.pvals,ylim=c(0,1),xlab="",ylab="",yaxt="n")
axis(2,las=2)
abline(h=0.05,lty=3)
means <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.pvals,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=mean)
points(means[,1],means[,2],col="red",pch=19)
points(means[,1],means[,2],col="red",type="l")
stdevs <- aggregate(RANDOMIZED.DSP.summary.df2 $L.DSP.LE.pvals,by=list(RANDOMIZED.DSP.summary.df2 $radius),FUN=sd)
arrows(means[,1],means[,2]+ stdevs[,2],means[,1],means[,2]- stdevs[,2],length=0.0,angle=90,code=3,col="red")
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.pvals,pch=19)
points(DSP.summary.df$radius, DSP.summary.df$L.DSP.LE.pvals,pch=19,type="l")
mtext("spatial scale(m)",side=1,line=2.3,cex= label.cex)
mtext("DSP p-value",side=2,line=3,cex= label.cex)
points(bs.pval.sig.pvals,rep(1,length(bs.rsquared.sig.pvals)),pch="*",cex=1)

