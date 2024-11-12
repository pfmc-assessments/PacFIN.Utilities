#' Get the ageing method used for the age that is agreed to be the best age
#'
#' Each age read will have an associated age method. This function will attempt
#' to find what ageing method was used for the age thought to represent the
#' best estimate of the age of the otolith.
#'
#' @details
#' When more than one age estimate matches what is thought to be the best age,
#' then the ageing methods for all ages that match the best age will be
#' returned as a single entry separated by "--". Most often, double reads are performed using the same method but there are cases, e.g., CARE exchanges or research questions on differences between methods, when a single otolith is read using multiple methods. For example, some petrale sole otoliths were read using both surface and break and burn reads.
#'
#' @inheritParams cleanPacFIN
#' @inheritParams cleanPacFIN
#'
#' @export
#' @author Kelli F. Johnson
#' @return
#' A vector of characters.
#' @examples
#' data <- data.frame(
#'   age1 = 1:10,
#'   age2 = 10,
#'   AGE_METHOD1 = "B",
#'   AGE_METHOD2 = "S",
#'   FINAL_FISH_AGE_IN_YEARS = 10
#' )
#' getAgeMethod(data)
#' # [1] "S"    "S"    "S"    "S"    "S"    "S"    "S"    "S"    "S"    "B--S"
getAgeMethod <- function(Pdata, verbose = TRUE) {
  # Codify the AGE_METHOD[0-9]+ columns to use the standardized ageing error
  # names returned from codify_age_method()
  text_original_methods <- Pdata %>%
    dplyr::select(dplyr::starts_with("AGE_M")) %>%
    unlist() %>%
    unique() %>%
    sQuote() %>%
    glue::glue_collapse(sep = ", ", last = " or ")
  Pdata <- dplyr::mutate(
    .data = Pdata,
    dplyr::across(dplyr::contains("AGE_METHOD"), ~ codify_age_method(.x))
  )
  # Indexing and labeling of relevant column names
  final_age_column_name <- grep_final_age(colnames(Pdata))
  age_reader_column_number <- grep("age[0-9]+$", colnames(Pdata))
  age_reader_integer_label <- gsub(
    "age",
    "",
    colnames(Pdata)[age_reader_column_number],
    ignore.case = TRUE
  )
  # A really complicated apply function to do row-wise operations
  used_method <- apply(Pdata, 1, function(x) {
    test_not_na <- !is.na(x[age_reader_column_number])
    test_match <- which(
      x[age_reader_column_number] == x[final_age_column_name]
    )
    if (
      all(is.na(x[grep("AGE_METHOD[0-9]+", names(x))])) |
        all(!test_not_na)
    ) {
      return(NA_character_)
    }
    if (length(test_match) > 0) {
      age_methods <- age_reader_integer_label[test_match]
    } else {
      age_methods <- age_reader_integer_label[test_not_na]
    }
    if (length(age_methods) > 0) {
      return(paste0(
        sort(unique(x[paste0("AGE_METHOD", age_methods)])),
        collapse = "--"
      ))
    } else {
      return(NA_character_)
    }
  })
  # Add it to the internal data frame so it is included in the printed summary
  # even though Pdata is not returned, just used_method
  Pdata[, "age_method"] <- used_method

  if (verbose) {
    returned_methods_n <- table(used_method, useNA = "always")
    text_returned_methods_n <- glue::glue(
      "{names(returned_methods_n)} (n = {returned_methods_n})"
    ) %>%
      glue::glue_collapse(sep = ", ", last = " and ")
    cli::cli_bullets(c(
      " " = "{.fn getAgeMethod} summary information -",
      "i" = "Age methods were originally coded to {text_original_methods}",
      "i" = "Age methods are now coded to {text_returned_methods_n}"
    ))
    print(summaryAgeMethod(Pdata, verbose = TRUE))
  }

  return(used_method)
}

#' The number of samples per combination of ageing methods
#'
#' Uses [dplyr::count()] to summarize the number of samples across the various
#' ageing methods (i.e., `AGE_METHOD[0-9]+`) that were used in the passed data
#' frame. Sometimes the ageing method will differ between double reads or
#' between years.
#'
#' @inheritParams cleanPacFIN
#'
#' @author Kelli F. Johnson
#' @return
#' A data frame of counts (`n`) for each age method with the following columns:
#' * `AGE_METHOD[0-9]*`: where the number at the end of the column indicates
#'   which age reader, e.g., first, second, third, etc., the method comes from.
#'   This column contains a letter or number indicating the ageing method used.
#'   There will be one column per number of reads available, i.e., if at least
#'   one fish was read by four reader, then there will be four columns starting
#'   with `AGE_METHOD`.
#' * `Age method for best age`: provides data on all methods used across age
#'   readers, for example if both break and burn and surface read information is
#'   available for a given fish then this column will have `B--S` as an entry.
#'   This information is important because sometimes the best age is based off
#'   multiple methods and it is difficult to know which ageing error to assign
#'   the entry to.
#' * `n`: the number of samples available for the given methods listed in that
#'   row.
#'
#' If `verbose`, then text explaining the summary is also printed to the screen
#' prior to returning the table.
#'
summaryAgeMethod <- function(Pdata, verbose = FALSE) {
  if (verbose) {
    cli::cli_bullets(c(
      "i" = "Number of samples (n) per combinations of ageing methods"
    ))
  }
  old_column_name <- c(
    "Age method for best age" = "age_method"
  )
  Pdata |>
    dplyr::select(dplyr::matches("AGE_METHOD", ignore.case = TRUE)) |>
    dplyr::count(dplyr::across(dplyr::everything())) |>
    dplyr::rename(dplyr::any_of(old_column_name))
}
