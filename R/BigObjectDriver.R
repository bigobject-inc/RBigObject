#' Class BigObjectDriver with constructor BigObject.
#'
#' An BigObject driver implementing the R database (DBI) API.
#' This class should always be initialized with the \code{BigObject()} function.
#' It returns a singleton that allows you to connect to BigObject.
#'
#' @export
#' @keywords internal
setClass("BigObjectDriver",
  contains = "DBIDriver"
)

#' @rdname BigObjectDriver-class
setMethod("dbUnloadDriver", "BigObjectDriver", function(drv, ...) {
  TRUE
})

#' @rdname BigObjectDriver-class
#' @export
setMethod("dbIsValid", "BigObjectDriver", function(dbObj) {
  TRUE
})

