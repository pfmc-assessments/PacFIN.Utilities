#' Table of Sample Size By Fleet and Year
#'
#' Create a table of samples by fleet and year to
#' be included in an assessment document.
#'
#' @template Pdata
#' @template fname
#' @template strat
#' @param comps Specify whether to calculate the length or age samples.
#' The default is to calculate the number of length samples.
#' @param remove_yrs A vector of years to remove from the data before
#' summarizing sample sizes. The default of \code{NULL} leads to no
#' sub setting of the data.
#' 
#' @return
#' A table is written to a csv file as specified in \code{fname} and the data frame
#' is also returned as an object invisibly.
#' 
#' @author Chantel Wetzel
#' @export

tableSample <- function(Pdata, fname = NULL, strat = NULL, comps = "LEN", remove_yrs = NULL) {

	if(is.null(strat)) {
		Pdata$strat = Pdata$SOURCE_AGID
	} else{
		if(length(strat) == 1) { 
			Pdata$strat = Pdata[,strat] }
		if(length(strat) == 2) { 
			Pdata$strat = paste(Pdata[,strat[1]], Pdata[,strat[2]], sep = ".") }
		if(length(strat) == 3) { 
			Pdata$strat = paste(Pdata[,strat[1]], Pdata[,strat[2]], Pdata[,strat[3]], sep = ".") }
	}

	if (comps == "LEN"){
		temp = Pdata[!is.na(Pdata$FISH_LENGTH),]
	}
	
	if (comps == "AGE"){
		temp = Pdata[Pdata$age != -1,]
	}

	if (!comps %in% c("LEN", "AGE")){
		message ("Input to comps should be equal to LEN or AGE. ")
		break()
	}

	if(!is.null(remove_yrs)){
		temp = temp[!temp$SAMPLE_YEAR %in% remove_yrs, ]
	}

	Ntow  = table(temp$SAMPLE_YEAR, temp$strat, !duplicated(as.character(temp$SAMPLE_NO)))[,,"TRUE"]
	Nfish = table(temp$SAMPLE_YEAR, temp$strat, !duplicated(as.character(temp$SAMPLE_NO)))[,,"FALSE"] 

	samples = rownames(Ntow); names = "Year"
	for (a in colnames(Ntow)){
		get = cbind(Ntow[,a], Nfish[,a])
		samples = cbind(samples, get)
		names = c(names, paste0(a, ".tows"), paste0(a, ".fish"))
	}
	colnames(samples) = names	

	if (is.null(fname)){
		filename = paste0(getwd(),"/fishery_", comps, "_samples.csv")
	} else {
		filename = fname
	}

	write.csv(samples, file = filename, row.names=FALSE)
  return(invisible(samples))
}