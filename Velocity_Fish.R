rm(list=ls(all=TRUE)) 
install.packages("weights") # Remove the # sing the first time
install.packages("scam") # Remove the # sing the first time
library(weights)
library(scam)
DB <- read.table("file name", sep="\t", header=T) 
summary(DB)
("Fish_name" <- DB[which(DB[,"Species"]==""Fish_name""), ])
(Availability <- DB[which(DB[,"Species"]=="Available"), ])
(Weights"Fish_name" <- log("Fish_name"[,"N.Fish"]+1))
summary("Fish_name")
summary(Availability)
## This is an example for one single variabel and size class. Change the colum names and settings if necessary
help(wtd.hist) # Remove the # sing to see the function help
Breaks <- c(0, 0.1, seq(0.08, max(DB[,"Velocity"]), length=5)) 
windows()
op <- par(mfrow=c(1,2))
AvailabilityHist <- hist(Availability[, "Velocity"], breaks = Breaks, col="darkgray")
UseWeightedHist <- wtd.hist("Fish_name"[, "Velocity"], breaks = Breaks, weight = Weights"Fish_name", col="darkgray")
par(op)
help(smooth.spline) 
AvailabilityHistSpline <- smooth.spline(x=AvailabilityHist$mids, y = AvailabilityHist$counts, spar=0.36, all.knots=FALSE) # Modify df, spar, all.knots and nknots to adjust the desired curve. Check help(smooth.spline) to select adequate values
(SimulatedAvailability <- predict(AvailabilityHistSpline, seq(0, max(DB[,"Velocity"]), by=0.01))) ## This is the smooth curve for availability
UseHistSpline <- smooth.spline(x=UseWeightedHist$mids, y = UseWeightedHist$counts, spar=0.62, all.knots=FALSE ) # Modify df, spar, all.knots and nknots to adjust the desired curve. Check help(smooth.spline) to select adequate values
(SimulatedUse <- predict(UseHistSpline, seq(0, max(DB[,"Velocity"]), by=0.01))) ## This is the smooth curve for use
windows()
op <- par(mfrow=c(1,3))
plot(SimulatedAvailability, col="black")
plot(SimulatedUse, col="black")
PreferenceSpline <- smooth.spline(x=AvailabilityHist$mids, y = UseWeightedHist$counts/AvailabilityHist$counts, spar=0.6, all.knots=FALSE) # Modify df, spar, all.knots and nknots to adjust the desired curve Check help(smooth.spline) to select adequate values
(SimulatedPreference <- predict(PreferenceSpline, seq(0, max(DB[,"Velocity"]), by=0.01))) ## This is the smooth curve for availability
plot(SimulatedPreference, ylim=c(0,0.3), col="azure4")
par(op)
