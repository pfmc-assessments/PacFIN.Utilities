#' Calculate von Bertalanffy Growth Parameters
#' 
#' Calaculate the von Bertalanffy growth parameters and compare them to
#' parameters estimated when outliers are removed from the data set.
#' Outliers are determined using standard deviations
#' (i.e., \code{sdFactor} input parameter).
#'
#' @template Pdata
#' @param Par A list of five initial or externally estimated parameter values for the
#' von Bertalanffy growth function. List entries should be named as follows:
#' "K", "Linf", "L0", "CV0", "CV1".
#' If they aren't named, then it is imperative that they are in the previous order.
#' Each list element, i.e., vector, can have multiple values specific to each sex type
#' present in your data. These vector elements must be named and the length of all
#' vectors must be the same. A typical example for a list element would be
#' \code{K = c(F = 0.10, M = 0.20, U = 0.15)} for female, male, and unsexed fish.
#' If each vector within the list is only made up of a single element, it doesn't need
#' to be named and each sex type present in the data will use the same value.
#' @param len_col Name of the length column to use for comparisons (default = "lengthcm").
#' @param age_col Name of the age column to use for comparisons (default = "age").
#' @param sex_col Name of the sex column to use for comparisons (default = "SEX").
#' If the value is \code{NULL}, then all aged fish are included in a single analysis rather
#' than separating data by available sex information.
#' @param mult A multiplier to convert lengths (default value 1). This multiplier is used
#' to scale the input parameters if the length column is a different unit other than cm.
#' In reality one really shouldn't need to use this because you can make a column of
#' lengths in the correct units and supply that column name to \code{len_col}.
#' @param keepAll Option to change inconsistent lengths to a value of \code{NA},
#' which is only done if \code{keepAll = FALSE}. The default is to not change any values.
#' @param sdFactor Option to control the threshold of the estimated low and high bound
#' that is compared to when throwing out data. Default value is 4 standard deviations
#' away from the mean prediction.
#' @param Optim A logical value specifying whether or not to internally solve
#' for the parameter estimates or use \code{Par} as input parameter to the growth function
#' for predictions.
#' @param precision An integer providing the precision for the predicted lengths or the number
#' of digits the results will have after passing them through
#' \code{\link{round}(x, digits = precision)}.
#' @template verbose
#'
#' @return Returns a combined dataset in PacFIN format.
#' 
#' @export
#' @author Chantel Wetzel, Vlada Gertseva, James Thorson
#' @examples
#' pdata <- data(XMPL.BDS)
#' pdata[, "FISH_LENGTH_TYPE"] <- ifelse(pdata[, "FISH_LENGTH_TYPE"] == "FALSE", "F", "T")
#' pdata[, "UNK_WT"] <- NA
#' pdata <- cleanPacFIN(pdata, keep_length_type = "F")
#' test <-  checkLenAge(pdata)
#' head(test)
###################################################################
checkLenAge <- function(Pdata,
  Par = list(K = 0.13, Linf = 55, L0 = 15, CV0 = 0.10, CV1 = 0.10),
  len_col = "lengthcm", age_col = "age", sex_col = "SEX",
  mult = 1, keepAll = TRUE, sdFactor = 4, Optim = TRUE,
  precision = 0, verbose = FALSE, dir = NULL) {

  #### Initialize the three new columns
  Pdata$Lhat_pred <- NA
  Pdata$Lhat_low <- NA
  Pdata$Lhat_high <- NA

  #### Check sex-specific information
  if (is.null(sex_col)) {
    sex_col <- "ignore"
    Pdata[, sex_col] <- "A"
  }
  sex_vec <- unique(Pdata[, sex_col])

  ####  Check inputs
  if (!is.null(names(Par))) {
    new <- Par[c("K", "Linf", "L0", "CV0", "CV1")]
    stopifnot("Par not found" = length(new) == length(Par))
    Par <- new
    rm(new)
  }
  if (length(Par[[1]]) > 1) {
    if (var(unlist(lapply(Par, length))) != 0) {
      stop("Must be one named entry in each vector element of the list Par\n",
        "for each unique sex type in the data, e.g.,\n",
        paste(sex_vec, collapse = ", "))
    }
    if (!all(sex_vec %in% names(Par[[1]]))) {
      stop("The data contains the following values for sexes,\n",
        paste(sex_vec, collapse = ", "), "\n",
        " which must match names of the parameter vectors in the list Par, e.g.,\n",
        paste(unique(unlist(lapply(Par, names))), collapse = ", ")
      )
    }
    # Ensure order of parameters within each vector are ordered by sex
    Par <- lapply(Par, "[", sex_vec)
  }
  stopifnot("length column not found" = len_col %in% colnames(Pdata))
  stopifnot("age column not found" = age_col %in% colnames(Pdata))

  #### For each sex
  for (s in seq_along(sex_vec)) {
    use_data <- !is.na(Pdata[, len_col]) &
                !is.na(Pdata[, age_col]) &
                Pdata[, age_col] != -1 &
                Pdata[, sex_col] %in% sex_vec[s]

    if (length(Par[[1]]) > 1) {
        pars_in <- c(
          Par[[1]][sex_vec[s]],
          Par[[2]][sex_vec[s]] * mult,
          Par[[3]][sex_vec[s]] * mult,
          Par[[4]][sex_vec[s]],
          Par[[5]][sex_vec[s]])
    } else {
        pars_in <- c(Par[[1]][1], Par[[2]][1]*mult, Par[[3]][1]*mult, Par[[4]][1], Par[[5]][1])
    }

    if (Optim) {
      ests <- optim(fn = nwfscSurvey::GetVB.fn,
                  par = log(pars_in),
                  hessian = FALSE,
                  Ages = Pdata[use_data, age_col],
                  Lengths = Pdata[use_data, len_col])$par
    } else {
        ests <- pars_in
    }
    estsall[s, -1] <- exp(ests)
    Pred <- nwfscSurvey::GetVB.fn(
      Par = ests,
      ReturnType = "Pred",
      sdFactor = sdFactor,
      Ages = Pdata[use_data, age_col],
      Lengths = Pdata[use_data, len_col])

    Pdata[use_data, c("Lhat_low","Lhat_pred", "Lhat_high")] <- round(Pred, precision)
  }

  if (!keepAll) {
    remove <- which(
      Pdata[, len_col] > Pdata[, "Lhat_high"] |
      Pdata[, len_col] < Pdata[, "Lhat_low"])
    all <- Pdata
    Pdata[remove, len_col] <- NA
  }

  #### Summary information that is saved to the disk
  if (!is.null(dir)) {
    dir <- normalizePath(dir)
    dir.create(dir, showWarnings = FALSE)

    # Estimate pars again b/c some data may be removed if !keepAll
    tempdata <- Pdata[
      !is.na(Pdata[, len_col]) &
      !is.na(Pdata[, age_col]) &
      Pdata[, age_col] != -1, ]
    estsall <- data.frame(Sex = sex_vec,
      do.call("rbind", tapply(seq(NROW(tempdata)), tempdata[, sex_col],
      function(x) exp(optim(fn = nwfscSurvey::GetVB.fn,
                  par = log(pars_in),
                  hessian = FALSE,
                  Ages = tempdata[x, age_col],
                  Lengths = tempdata[x, len_col])$par)
    )))
    colnames(estsall)[-1] <- names(Par)
    estsall <- estsall[, c("Sex", "L0", "Linf", "K", "CV0", "CV1")]
    colnames(estsall) <- c("Sex", "$L_0$", "$L_{Inf}$", "$K$", "$CV_{young}$", "$CV_{old}$")
    x <- knitr::kable(estsall, format = "latex",
      label = "PacFIN_vonBpars", escape = FALSE, booktabs = TRUE,
      caption = paste0(
        "Estimates of von Bertalanffy growth parameters in terms of ",
        "length at age-0 ($L_0$), rather than age at which growth is zero, and fit to ",
        "fishery-dependent data provided by \\gls{pacfin}. Estimates ",
        ifelse(all(estsall[, "Sex"] == "A"), "", "are sex-specific (row) and "),
        "include $L_0$, length at maximum age ($L_{Inf}$), growth rate ($K$), and ",
        "coefficients of variation at young ($CV_{young}$) and old ages ($CV_{old}$).",
        ifelse(keepAll, " Data used to fit the model included outliers.", ""))
    )
    writeLines(x, file.path(dir, "PacFIN_vonBpars.tex"))
    utils::write.table(estsall, file = file.path(dir, "PacFIN_vonBpars.csv"),
      sep = ",", row.names = FALSE)
   tempdata <- tempdata[, c(len_col, age_col, sex_col)]
   colnames(tempdata) <- c("Length_cm", "Age", "Sex")
   pars <- unlist(estsall[1, c(3, 4, 2, 5, 6), drop = TRUE])
   names(pars) <- NULL
   latage <- PlotVarLengthAtAge.fn(dat = tempdata, parStart = pars,
     dir = dir, main = "PacFIN", ageBin = 1,
     bySex = !all(estsall[, "Sex"] == "A"),
     estVB = TRUE, legX = "bottomleft", dopng = TRUE)
  }

  Pdata <- Pdata[, !grepl("ignore", colnames(Pdata))]
  return(Pdata)
}
