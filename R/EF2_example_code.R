##############################################################################
#
# Expansion 2 example.  Example catch file is from DBRK, supplied by Vlada.

Catch = read.csv("Catch.DBRK.csv", skip=1, as.is=T)

# Rename Catch columns

names(Catch) = c("Year", "CA.TWL","OR.TWL","WA.TWL","CA.NONTWL","OR.NONTWL","WA.NONTWL")

# Encode the two gears to use in this example
# Note that the state value will be pasted to the usegear to create the full name.

Pdata$usegear = "NONTWL"
Pdata$usegear[Pdata$geargroup %in% c("TWL","TWS")] = "TWL"

# No WA.NONTWL in example Pdata (from PTRL), remove column 7 for this example to run.

Catch = Catch[,-7]

# Get the expansion, converting the metric tonnes of catch to pounds

Pdata = getExpansion_2(Pdata, Catch, Convert=T, maxExp=0.9)

#
# That's All, Folks!
#
##############################################################################
