#' Class BigObjectConnection.
#'
#' \code{BigObjectConnection.} objects are usually created by
#' [DBI::dbConnect()]
#'
#' @import methods DBI
#' @export
#' @keywords internal
setClass("BigObjectConnection",
  contains = "DBIConnection", 
  slots = list(
    backend = "MySQLConnection", 
    has_con = "logical"
  )
)

#' @import methods DBI
#' @export
#' @rdname BigObjectConnection-class
setMethod("dbDisconnect", "BigObjectConnection", function(conn, ...) {
  if (dbIsValid(conn)) {
    dbDisconnect(conn@backend)
    eval.parent(substitute(conn@has_con <- FALSE))
  } else {
	warning("The connection has been disconnected.")
  }
  invisible(TRUE)
})

#' @export
#' @rdname BigObjectConnection-class
setMethod("dbGetInfo", "BigObjectConnection", function(dbObj, what="", ...) {
  #dbGetInfo(dbObj@backend)
  dbIsValid(dbObj)
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
  #dbIsValid(dbObj@backend)
  if (dbObj@has_con) {
    backend_is_corrupted <- FALSE
    tryCatch ({dbGetQuery(dbObj@backend, "SHOW CONFIG")}, error=function(e){backend_is_corrupted <- TRUE }) 
    if (backend_is_corrupted) {
      dbObj@has_con <- FALSE
      eval.parent(substitute(dbObj@has_con <- FALSE))
    }  
  }
  dbObj@has_con 
})

##' @export
#DBI::dbGetQuery
