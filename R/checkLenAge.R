###################################################################
#
#
#' Calculate the von Bertanlaffy growth funciton and compare length
#' age value to those estimated with the option to throw out records
#' that are outside a specified sd band.
#'
#' 
#' \subsection{Workflow}{
#' Intended to be run after \code{\link{cleanPacFIN}}.
#' }
#' 
#' @param Pdata a PacFIN dataset
#' @param Par initial or externally estimated parameter values for the vonB
#' growth function where the order are ("k", "Linf", "Lmin", "CV young", "CV old").
#' Can define a list with multiple values by sex where the expected order is 
#' c(k.vec, Linf.vec, Lmin.vec, CVyoung.vec, CVold.vec) where each vector is c(female, male, unsex)
#' parameter value.
#' The lengths are input in cms for ease of use and are converted internally to mm.
#' @param keepAll Option to NA inconsistent lengths
#' @param sdFactor Option to control the threshold of the estimated low and high bound
#' that is compared to when throwing out data. Default value is 4 stan. devs. away from the 
#' predictions.
#' @param Optim Control whether to internally solve for the parameter estimates or use the input
#' parameters.
#'
#' @return Returns a combined dataset in PacFIN format.
#' 
#' @export
#'
#' @details
#' 
#' 
#'
#' @author Chantel Wetzel, Vlada Gertseva, James Thorson
#
###################################################################
checkLenAge = function(Pdata, Par =  list( 0.13, 55, 15, 0.10, 0.10), keepAll = TRUE, sdFactor = 4, Optim = TRUE)
{
    Pdata$Lhat_pred = NA
    Pdata$Lhat_low = NA
    Pdata$Lhat_high = NA

    if(is.null(Pdata$lengthcm)) {
        cat( "Need to run the cleanPacFIN first.")
    }


    VbFn = function(Par, Ages, Lengths, ReturnType = "NLL")
    {
        K    = exp(Par[1])
        Linf = exp(Par[2])
        L0   = exp(Par[3])
        CV0  = exp(Par[4])
        CV1  = exp(Par[5])
        
        Lhat = Linf - (Linf-L0) * exp(-Ages*K)
        CV = CV0 + (CV1 - CV0) * (Lhat-L0)/Linf
        SD = CV * Lhat 
        Lhat_low = Linf - (Linf-L0) * exp(-Ages*K) - sdFactor*SD
        Lhat_high = Linf - (Linf-L0) * exp(-Ages*K) + sdFactor*SD
        NLL = -1 * sum(dnorm(Lengths, mean=Lhat, sd=SD, log=TRUE), na.rm=TRUE)

        if(ReturnType == "NLL" ) { Return = NLL }
        if(ReturnType == "Pred") { Return = cbind( 'Lhat_low'=Lhat_low, 'Lhat_pred'=Lhat, 'Lhat_high'=Lhat_high ) }
        return(Return)
    }

    # Calculate female length-at-age
    get_sex = c("F", "M", "U") %in% unique(Pdata$SEX)
    sex_vec = c("F", "M", "U")[get_sex]
    for (s in 1:length(sex_vec)){
        use_data = !is.na(Pdata$length) & Pdata$age != -1 & Pdata$SEX == sex_vec[s]

        if (length(Par[[1]]) > 1){ 
            pars_in = log(c(Par[[1]][s], Par[[2]][s]*10, Par[[3]][s]*10, Par[[4]][s], Par[[5]][s]))
        } else {
            pars_in = log(c(Par[[1]][1], Par[[2]][1]*10, Par[[3]][1]*10, Par[[4]][1], Par[[5]][1]))
        }

        if (Optim == TRUE){
            Opt = optim(fn=VbFn, 
                        par=pars_in, 
                        hessian=FALSE, 
                        Ages=Pdata[use_data,'age'], 
                        Lengths=Pdata[use_data,'length'])
            ests = Opt$par
        } else {
            ests = pars_in 
        }

        Pred = VbFn(Par=ests, 
                    ReturnType="Pred",  
                    Ages=Pdata[use_data,'age'], Lengths=Pdata[use_data,'length']) 

        Pdata[use_data, c("Lhat_low","Lhat_pred", "Lhat_high")] = round(Pred,0)
    }

    if (!keepAll){
        remove = which(Pdata[,'length'] > Pdata[,'Lhat_high'] | Pdata[,'length'] < Pdata[,'Lhat_low'])
        Pdata$length[remove] = Pdata$age[remove] = NA
    }
    return(Pdata)
}