

# From http://adv-r.had.co.nz/beyond-exception-handling.html
condition <- function(subclass, message, call = sys.call(-1), ...) {
  structure(
    class = c(subclass, "condition"),
    list(message = message, call = call, ...)
  )
}

# errors is a vector of strings (can be more than one error string)
experiment_definition_validation_error <- function(errors) {
  msg <- paste0("Invalid experiment definition: ", paste(errors, collapse = "\n"))
  condition(c("experiment_definition_validation_error", "error"),
    message = msg,
    errors = errors
  )
}
