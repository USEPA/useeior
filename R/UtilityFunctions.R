#' Join strings with slashes
joinStringswithSlashes <- function(...) {
  items <- list(...)
  str <- sapply(items, paste, collapse = '/')
  str <- tolower(str)
  return(str)
}
