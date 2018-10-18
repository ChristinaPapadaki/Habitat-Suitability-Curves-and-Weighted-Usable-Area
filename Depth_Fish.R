rm(list=ls(all=TRUE)) ## neteja les variables
library(weights)
library(scam)
DB <- read.table("File_name", sep="\t", header=T) # Change the route if necessary
summary(DB)
("Fish_name" <- DB[which(DB[,"Species"]==""Fish_name""), ])
(Availability <- DB[which(DB[,"Species"]=="Available"), ])
(Weights"Fish_name" <- log("Fish_name"[,"N.Fish"]+1))
## This is an example for one single variabel and size class. Change "Fish_name"  and settings if necessary   
# help(wtd.hist) # Remove the # sing to see the function help
Breaks <- seq(0, max(DB[,"Depth"]), length=7) # length=5 determines the number of bins
windows()
op <- par(mfrow=c(1,2))
AvailabilityHist <- hist(Availability[, "Depth"], breaks = Breaks, col="darkgray")
UseWeightedHist <- wtd.hist("Fish_name"[, "Depth"], breaks = Breaks, weight = Weights"Fish_name", col="darkgray")
par(op)
help(smooth.spline) # Add the # sing to disable this call
AvailabilityHistSpline <- smooth.spline(x=AvailabilityHist$mids, y = AvailabilityHist$counts, spar=0.5, all.knots=FALSE) # Modify df, spar, all.knots and  to adjust the desired curve. Check help(smooth.spline) to select adequate values
(SimulatedAvailability <- predict(AvailabilityHistSpline, seq(0.08, max(DB[,"Depth"]), by=0.01))) ## This is the smooth curve for availability
UseHistSpline <- smooth.spline(x=UseWeightedHist$mids, y = UseWeightedHist$counts, spar=0.5, all.knots=FALSE) # Modify df, spar, all.knots and nknots to adjust the desired curve. Check help(smooth.spline) to select adequate values
(SimulatedUse <- predict(UseHistSpline, seq(0.08, max(DB[,"Depth"]), by=0.01))) ## This is the smooth curve for use
windows()
op <- par(mfrow=c(1,3))
plot(SimulatedAvailability, col="black")
plot(SimulatedUse, col="black")
## Preference:
PreferenceSpline <- smooth.spline(x=AvailabilityHist$mids, y = UseWeightedHist$counts/AvailabilityHist$counts, spar=0.1, all.knots=FALSE) # Modify df, spar, all.knots and nknots to adjust the desired curve. Check help(smooth.spline) to select adequate values
(SimulatedPreference <- predict(PreferenceSpline, seq(0.08, max(DB[,"Depth"]), by=0.01))) ## This is the smooth curve for availability
plot(SimulatedPreference, col="azure4")
par(op)
