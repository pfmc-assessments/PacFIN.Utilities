#' Get password from the user if [interactive()]
#' @author Kelli F. Johnson
#' @return
#' Invisibly returns a string if [interactive()]. If it is not an interactive R
#' session, then a [stop()] message is displayed on the console.
ask_password <- function() {
  if (interactive()) {
    password <- readline(
      prompt = "Enter PacFIN password without quotes followed by a return\n"
    )
  } else {
    stop("You must pass your password via the input argument `password`.")
  }
  return(invisible(password))
}
