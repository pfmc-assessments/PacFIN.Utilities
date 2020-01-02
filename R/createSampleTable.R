#' Create table of samples by fleet and year to 
#' be included in US West Coast groundfish assessments
#' 
#' todo: document
#' 
#' @param Pdata data frame
#' @param fname user specified file name (.csv) for the tables
#' @param strat calculate the sample sizes using the input columns ( c("fleet","usegear"))
#' @param comps specify whether to calculate the length or age samples
#' @param remove_yrs remove years of data for inclusion in the table.
#' 
#' @author Chantel Wetzel

createSampleTable <- function(Pdata, fname = NULL, strat = NULL, comps = "LEN", remove_yrs = NULL) {

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
		temp = Pdata[!is.na(Pdata$age),]
	}

	if (!comps %in% c("LEN", "AGE")){
		message ("Input to comps should be equal to LEN or AGE. ")
		break()
	}

	if(!is.null(remove_yrs)){
		temp = temp[!Pdata$SAMPLE_YEAR %in% remove_yrs, ]
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
}