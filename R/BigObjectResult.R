#' Class BigObjectResult
#'
#' BigObject's query results class.  This classes encapsulates the result of an SQL
#' statement (either \code{select} or not).
#'
#' @export
#' @keywords internal
setClass("BigObjectResult",
  contains = "DBIResult",
  slots = list(
    bak_res = "MySQLResult",
    sql = "character"
  )
)

#' @rdname BigObjectResult-class
#' @export
setMethod("dbIsValid", "BigObjectResult", function(dbObj) {
  dbIsValid(dbObj@bak_res) 
})

