
##############################################################################
#
#  getSeason adds a field for season to the data.  Several seasonal schemes
#  may be provided, including the Petrale seasons ( 1 = winter months, 2 else ).
#
#  Other schemes may be provided as needed.
#
#  The yearUp and yearDown can be used to provide a list of SAMPLE_MONTHS
#  (1 through 12) for which to adjust the fishyr (the fishing year) up or down.
#  In other words, the winter months might belong to the following year, or to
#  the previous year:
#
#      yearUP = c(11,12)    or    yearDown = c(1,2)
#
##############################################################################

getSeason = function ( Pdata, season_type=-1, yearUp=NULL, yearDown=NULL, plotResults=F) {

  cat( "\nDefault season = 1\n\n")

  Pdata$season = 1

  if ( season_type == 0 ) {

    cat( "Assigning season from SAMPLE_MONTH\n\n")

    Pdata$season = as.numeric(Pdata$SAMPLE_MONTH)

  } # End if

  # Petrale seasons

  if ( season_type == 1 ) {

    cat("Assigning season ala Petrale; winter is season 1, summer is 2.\n\n")

    Pdata$season = 2
    Pdata$season[Pdata$SAMPLE_MONTH %in%  c(11,12,1,2)] = 1

  } # End if Petrale

  if (! is.null(yearUp)) {

    Pdata$fishyr[Pdata$SAMPLE_MONTH %in% yearUp] =
    Pdata$fishyr[Pdata$SAMPLE_MONTH %in% yearUp] + 1

    cat("Incremented fishyr for months", yearUp, "to the next year.\n\n")

  } # End if yearUp

  if (! is.null(yearDown)) {

    Pdata$fishyr[Pdata$SAMPLE_MONTH %in% yearDown] =
    Pdata$fishyr[Pdata$SAMPLE_MONTH %in% yearDown] - 1

    cat("Decremented fishyr for months", yearDown, "to the previous year.\n\n")

  } # End if yearDown

  if ( plotResults ) {

    nSeas = length(unique(Pdata$season))
    tmp = xtabs(Pdata$FREQ ~ Pdata$season + Pdata$fishyr)
    barplot(tmp, col=rainbow(nSeas), legend.text=paste("Season", rownames(tmp)),
            main=unique(Pdata$SPID), xlab="Year", ylab="Samples")

  } # End if

  return( Pdata )

} # End getSeason
