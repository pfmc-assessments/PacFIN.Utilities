#' Code ageing methods to standardized character strings
#'
#' Code ageing error methods to a set of standard character strings because
#' some agencies use different codes for the same method. For example, break
#' and burn can be "B", "BB", or 1.
#'
#' @details
#' ## Washington Department of Fish and Wildlife
#' * B-break and burn;
#' * E-enhanced break and burn;
#' * L-length;
#' * M-more than one age method was used, e.g., "B" and "S";
#' * N-not aged;
#' * O-optical scanner;
#' * X-sectioning
#'
#'  ## Oregon Department of Fish and Wildlife
#' * 1-break and burn;
#' * 2-surface;
#' * 3-scales;
#' * 4-thin section;
#' * 5-optical scanner;
#' * 6-length;
#' * 9-unable;
#' * 10-break and bake;
#'
#'  ## California Department of Fish and Wildlife
#' * B-break and burn;
#' * S-surface;
#' * T-thin section
#'
#' @param x A vector that can be a mix of integers and characters.
#' @export
#' @author Kelli F. Johnson
#' @return A vector of characters.
#' @examples
#' codify_age_method(c("B", "1", "AA", NA))
codify_age_method <- function(x) {
  all_NA <- c(9, "N", "U", "UNK")
  out <- dplyr::case_when(
    x == 1 ~ "B",
    x == "B" ~ "B",
    x == "BB" ~ "B",
    x == "E" ~ "B",
    x == 2 ~ "S",
    x == "S" ~ "S",
    x == "SR" ~ "S",
    x == 3 ~ "3",
    x == 4 ~ "T",
    x == "T" ~ "T",
    x == "TS" ~ "T",
    x == "X" ~ "T",
    x == 5 ~ "O",
    x == "O" ~ "O",
    x == 6 ~ "L",
    x == "L" ~ "L",
    x == "M" ~ "M",
    x == 10 ~ "B",
    x %in% all_NA ~ NA_character_,
    is.na(x) ~ NA_character_,
    TRUE ~ NA_character_
  )

  unknowns <- table(
    x[(is.na(out) & !is.na(x)) & !x %in% all_NA],
    useNA = "ifany"
  )
  error_message <- glue::glue("'{names(unknowns)}' (n = {unknowns})")
  if (length(unknowns) > 0) {
    message(
      "The following unmatched values were found n times in",
      " `codify_age_method()`:\n",
      glue::glue_collapse(error_message, "\n")
    )
  }
  return(out)
}
