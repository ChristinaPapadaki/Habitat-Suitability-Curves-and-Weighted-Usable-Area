install.packages("RSAGA") ## Download from the web the set of functions (package) that allow to manage directly ascii files
# Once the code has been run once place a # sign before the previous row. That code must be run just once per computer
rm(list=ls(all=TRUE)) ## clean any variable in the workspace
library(RSAGA) ## Load the package RSAGA in the current session
## Load the depth curve
DepthCurve <- read.table("Filename_Depthcurve") ## In any other computer routes must be changed 
## Check the depth curve
windows()
plot(DepthCurve[,"Depth"],DepthCurve[,"HS"],type="l",ylim=c(0,1))
## Load the Velocity curve
VelocityCurve <- read.table("Filename_Velocitycurve",header=TRUE,sep="\t") ## In any other computer routes must be changed
## Check the Velocity curve
## Notice the HSC uses on Velocity to term the velocity instead of mean velocity
windows()
plot(VelocityCurve[,"Velocity"],VelocityCurve[,"HS"],type="l",ylim=c(0,1))
## Load the Substrate curve
## Notice the HSC need a numeric field called SubtrateCode and CoverCode
SubstrateCurve <- read.table("Filename_Substratecurve",header=TRUE,sep="\t") ## In any other computer routes must be changed
## Check the Substrate curve
windows()
plot(SubstrateCurve[,"SubstrateCode"],SubstrateCurve[,"HS"],type="l",ylim=c(0,1)) # Notice I'm using a code for the substrate classes
## Load the Cover curve
## Notice the HSC need a numeric field called SubtrateCode and CoverCode
CoverCurve <- read.table("Filename_Covercurve",header=TRUE,sep="\t") ## In any other computer routes must be changed
## Check the Cover curve
windows()
plot(CoverCurve[,"CoverCode"],CoverCurve[,"HS"],type="l",ylim=c(0,1)) # Notice I'm using a code for the cover classes
## The following codes are prepared for Mesochora Up. They must be changed for Tripotamos or if a different set of simulated flows in assessed
Flow.Codes <- c("Flow_names") ## These are the names of the files in my computer. I would prefer not to have two dots in the name of a file. ONe dot for the extension is enough.
Flow.Real <- c("Flow_values") ## These are the real values.
## These are the flows we are going to inspect
Flow <- Flow.Codes
## The resulting WUA will be storaged in the following matrix
Results <- mat.or.vec(length(Flow),6) ## It has coulmns corresponding to simulated flow, min., max., product, geometric mean and arithmetic mean.
## Here the column names of the matrix that will storage the results are changed
colnames(Results) <- c("flow", "min.", "max.", "product", "geometric", "arithmetic")
## Here the first column corresponding to the simulated flows is replaced by the real values
Results[,"flow"] <- Flow.Real
## Substrate and cover are common for every simulated flow so they are loaded just once
Substrate <- read.ascii.grid(file="Filename.txt", return.header = TRUE) ## Load Substrate
## Convert the ascii file in a matrix with xy and z values upon these matrix the habitat suitability will be assessed
Substrate.xyz <- grid.to.xyz(data=Substrate, varname = "Substrate")
Cover <- read.ascii.grid(file="Filename.txt", return.header = TRUE) ## Load Cover
## Convert the ascii file in a matrix with xy and z values upon these matrix the habitat suitability will be assessed
Cover.xyz <- grid.to.xyz(data=Cover, varname = "Cover")
## The for particule refers to a loop. Then the following code (i.e. the one between {}) will be repeated as many times as 'flows' have been stated
for(i in (1:length(Flow))) ## Length measures the number of elements in the vector Flow
{
## Read the corresponding depth and velocity ascci file and likewise convert them in a xyz matrix
Depth <- read.ascii.grid(file=paste("Filename/depth_",Flow.Codes[i],".txt",sep=""), return.header = TRUE)
Depth.xyz <- grid.to.xyz(data=Depth, varname = "Depth")
Velocity <- read.ascii.grid(file=paste("Filename_vel_",Flow.Codes[i],".txt",sep=""), return.header = TRUE)
Velocity.xyz <- grid.to.xyz(data=Velocity, varname = "Velocity")
## Interpolate the habitat suitability for every of the considered variables; depth, velocity, substrate and cover.
DepthSuitability <- approx(x = DepthCurve[,"Depth"],y = DepthCurve[,"HS"], xout = Depth.xyz[,"Depth"], method = "linear", rule = 2)$y
VelocitySuitability <- approx(x = VelocityCurve[,"Velocity"],y = VelocityCurve[,"HS"], xout = Velocity.xyz[,"Velocity"], method = "linear", rule = 2)$y
## Notice the HSC need a numeric field called SubtrateCode and CoverCode
SubstrateSuitability <- approx(x = SubstrateCurve[,"SubstrateCode"],y = SubstrateCurve[,"HS"], xout = Substrate.xyz[,"Substrate"], method = "linear", rule = 2)$y
CoverSuitability <- approx(x = CoverCurve[,"CoverCode"],y = CoverCurve[,"HS"], xout = Cover.xyz[,"Cover"], method = "linear", rule = 2)$y
## We storage everything in a single matrix. Upon this matrix we will calculate the min max prodct etc.
CurrentStepResults <- data.frame(X = Depth.xyz[,"x"] , Y = Depth.xyz[,"y"], Depth_HS = DepthSuitability, Velocity_HS = VelocitySuitability, Substrate_HS = SubstrateSuitability, Cover_HS = CoverSuitability)
## Depth or velocity can present no-data values. Consequently any suitability assigned in the remaining rows must be removed
CurrentStepResults[!complete.cases(CurrentStepResults),3:ncol(CurrentStepResults)] <- NA # NA means Not Available
## Now a suitability map for every partial suitability i.e. the one associated with depth then with velocity and so on is produced and storeged in the root folder
for(ii in 3:6) ## This number 3:6 refer to the columns in the object; CurrentStepResults
{
## This part generated the colours for every pixel
Colours <- as.character(cut(CurrentStepResults[,ii], breaks=c(0,0.2,0.4,0.6,0.8,1), labels = c("red","orange","yellow","green","green3"), include.lowest = TRUE, right = TRUE, dig.lab = 3, ordered_result = FALSE))
## Save a tiff image with the partial suitability for every input variable
tiff(paste("Filename",colnames(CurrentStepResults)[ii],"_flow_",Flow.Codes[i],".tif",sep=""), width = 7.5, height = 7.5, units="cm", res=800, compression = "lzw")
## parameters of the plot
op <- par(oma=c(0.15,0.15,0.1,0.1),mar=c(4.25,4.25,2,1)) ## (abajo,izq,arriba,derecha)
## plot xy data with the colours corresponding to the suitability
plot(CurrentStepResults[,"X"],CurrentStepResults[,"Y"],col=Colours,pch=15,cex=0.15,main=paste(colnames(CurrentStepResults)[ii]," - flow: ",as.character(Flow.Real[i]),sep=""),xlab="X",ylab="Y",bty="n",cex.main=0.975)
## Add the legend
legend("topleft", legend=c("0.0-0.2","0.2-0.4","0.4-0.6","0.6-0.8","0.8-1.0"), fill = c("red","orange","yellow","green","green3"), col = c("red","orange","yellow","green","green3"),border = c("red","orange","yellow","green","green3"),cex=0.7,bty = "n")
## Disable the parameters
par(op)
## Save the image
dev.off()
} ## Close the loop
## Once the partial suitability for every input variable has been calculated aggregated suitability can be calculates
## The funtion apply repeats the funtion at the end of the string for every row of the CurrentStepResults from 3 (depth) to 6 (cover)
Min. <- apply(CurrentStepResults[,3:6],1,min)
Max. <- apply(CurrentStepResults[,3:6],1,max)
Product <- apply(CurrentStepResults[,3:6],1,prod)
Geometric <- apply(CurrentStepResults[,3:6],1,function(x){prod(x)^(1/length(x))}) ## Interestingly the geometric mean is not by defoult not available in R
Arithmetic <- apply(CurrentStepResults[,3:6],1,mean)
## These results are added to the current results
CurrentStepResults <- data.frame(CurrentStepResults,Min.,Max.,Product,Geometric,Arithmetic)
## Now a suitability map for every aggregation method i.e. the min, the max, the product and so on is produced and produced and storaged in the root folder
for(ii in 7:11)
{
## This part generated the colours for every pixel
Colours <- as.character(cut(CurrentStepResults[,ii], breaks=c(0,0.2,0.4,0.6,0.8,1), labels = c("red","orange","yellow","green","green3"), include.lowest = TRUE, right = TRUE, dig.lab = 3, ordered_result = FALSE))
## Save a tiff image with the partial suitability for every input variable
tiff(paste("Filename",colnames(CurrentStepResults)[ii],"_flow_",Flow.Codes[i],".tif",sep=""), width = 7.5, height = 7.5, units="cm", res=800, compression = "lzw")
## parameters of the plot
op <- par(oma=c(0.15,0.15,0.1,0.1),mar=c(4.25,4.25,2,1)) ## (abajo,izq,arriba,derecha)
## plot xy data with the colours corresponding to the suitability
plot(CurrentStepResults[,"X"],CurrentStepResults[,"Y"],col=Colours,pch=15,cex=0.15,main=paste(colnames(CurrentStepResults)[ii]," - flow: ",as.character(Flow.Real[i]),sep=""),xlab="X",ylab="Y",bty="n",cex.main=0.975)
## Add the legend
legend("topleft", legend=c("0.0-0.2","0.2-0.4","0.4-0.6","0.6-0.8","0.8-1.0"), fill = c("red","orange","yellow","green","green3"), col = c("red","orange","yellow","green","green3"),border = c("red","orange","yellow","green","green3"),cex=0.7,bty = "n")
## Disable the parameters
par(op)
## Save the image
dev.off()
} ## Close the loop
## Then we save the CurrentStepResults in a .txt file. It can be opened in excel and queried. The route should be changed!!!
write.table(CurrentStepResults,paste("Filename",Flow.Codes[i],".txt",sep=""),row.names=FALSE,sep="\t")
#The WUA is claculated for every aggregation method.
WUACurrent <- apply(CurrentStepResults[,7:11],2,function(x){
CurrentX <- x[!is.na(x)]
sum(CurrentX[CurrentX>=0.5])})
## The current WUA for every aggregation method is storaged in the Results matrix
Results[i,2:ncol(Results)] <- WUACurrent
}## Close the loop
Results
## Then we save the overall results in a .txt file. It can be opened in excel and queried ploted whatever. The route should be changed!!!
write.table(Results,"Filename",row.names=FALSE,sep="\t")






