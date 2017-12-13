#' Class BigObjectConnection.
#'
#' \code{BigObjectConnection.} objects are usually created by
#' \code{\link[DBI]{dbConnect}}
#'
#' @import methods DBI
#' @export
#' @keywords internal
setClass("BigObjectConnection",
  contains = "DBIConnection", 
  slots = list(
    backend = "MariaDBConnection"
  )
)

#' @import methods DBI
#' @export
#' @rdname BigObjectConnection-class
setMethod("dbDisconnect", "BigObjectConnection", function(conn, ...) {
  if (dbIsValid(conn@backend)) {
    dbDisconnect(conn@backend)
  } else {
	warning("The connection has been disconnected.")
  }
  invisible(TRUE)
})

#' @export
#' @rdname BigObjectConnection-class
setMethod("dbGetInfo", "BigObjectConnection", function(dbObj, what="", ...) {
  #connection_info(dbObj@ptr)
  TRUE
})

#' @export
#' @rdname BigObjectConnection-class
setMethod("show", "BigObjectConnection", function(object) {
  #info <- dbGetInfo(object)
  cat("<BigObjectConnection>\n")
  #if (dbIsValid(object)) {
  #  cat("  Host:    ", info$host, "\n", sep = "")
  #  cat("  Server:  ", info$serverVersion, "\n", sep = "")
  #  cat("  Client:  ", info$client, "\n", sep = "")
  #  #cat("  Proto:   ", info$protocolVersion, "\n", sep = "")
  #  #cat("  ThreadId:", info$threadId, "\n", sep = "")
  #  #cat("  User:    ", info$user, "\n", sep = "")
  #  #cat("  ConType: ", info$conType, "\n", sep = "")
  #  #cat("  Db:      ", info$dbname, "\n", sep = "")
  #} else {
  #  cat("  DISCONNECTED\n")
  #}
})

#' @export
#' @rdname BigObjectConnection-class
setMethod("dbIsValid", "BigObjectConnection", function(dbObj) {
  dbIsValid(dbObj@backend)
})

##' @export
#DBI::dbGetQuery
