##############################################################################
#
# Example run of comps creation, start to finish.
#
##############################################################################

setwd("~/Desktop/PacFIN_Comps_Beta_V1")

source("PacFIN_Utilities.R")

# Get data
load("PacFIN.PTRL.bds.11.May.2011.dmp")
CALCOM = read.csv("PetraleCALCOM_Query2011.csv", as.is=T)

# Combine
Pdata = combineCALCOM(PacFIN.PTRL.bds.11.May.2011, CALCOM)

# Clean and filter for lengths

Pdata = cleanPacFIN(Pdata)

# Further filtering and preparation for expansions

Pdata = getState(Pdata)
Pdata = getSeason(Pdata, season_type=1, yearUp=c(11,12))
Pdata = getGearGroups(Pdata)

# Plot the length data

plotCleaned(Pdata)

# Get the level-1 expansion

Pdata = getExpansion_1(Pdata)

# Run the example for the second-level expansion

source("EF2_example_code.R", echo=T)

# set the final sample size, evaluate it and cap it at the 90th percentile value.

Pdata$Final_Sample_Size = Pdata$Expansion_Factor_1 * Pdata$Expansion_Factor_2
summary(Pdata$Final_Sample_Size)
Pdata$Final_Sample_Size = capValues(Pdata$Final_Sample_Size, maxVal = 0.90)
summary(Pdata$Final_Sample_Size)

# Filter the length data for ages and plot them, AFTER getting the Final_Sample_Size

AgeData = cleanAges(Pdata, minAge = 1)
plotCleanedAges(AgeData)


# From here, the steps for each comps file is:  get the comps, assign unsexed fish
# to be males or females, then write out the comps.

# Get the Length comps.  Let the sex ratio be a stratum-by-stratum value
# as seen in the data.

Lcomps = getComps(Pdata, Comps="LEN")
Lcomps = doSexRatio(Lcomps, findRatio=T)
Lcomps = writeComps(Lcomps, fname="Lcomps.csv", lbins=seq(0,90,5))

# Now get Age comps stratified by state and usegear as well as the usual stratifying fields.
# Assign sex ratio according to a ramp up from 0.5 to 1.

Acomps = getComps(AgeData, strat=c("state", "usegear"), Comps="AGE")

# Need (age) Bins and Rvector the same length

length(seq(0,40,4))
length(seq(0.5, 1, 0.1))
length(seq(0.5, 1, 0.05))
length(seq(0.5, 1, 0.051))

Acomps = doSexRatio(Acomps, Rvector = seq(0.5, 1, 0.051), Bins=seq(0,90,10))

writeComps(Acomps, fname=("Acomps.csv"))


# Now the Age-at-Length comps, stratified by state in addition to the usual stratifying fields.

AALcomps = getComps(AgeData, strat=c("state"), Comps="AAL")

# Sex ratio is 0.5 for unsexed fish up to 15 cm, 0.7 until 75 cm, then 1.

AALcomps = doSexRatio(AALcomps, Rvector=0.7, ratioU = 0.5, maxsizeU = 15, GTsizeU = 75)

writeComps(AALcomps, fname="AALcomps.csv", lbins=seq(0,90,5))

#
# That's All, Folks!
#
##############################################################################
