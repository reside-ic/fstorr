`%||%` <- function(x, y) { # nolint
  if (is.null(x)) y else x
}


squote <- function(x) {
  sprintf("'%s'", x)
}
