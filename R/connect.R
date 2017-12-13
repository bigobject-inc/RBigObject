#' @include BigObjectConnection.R
NULL

#' Connect/disconnect to a BigObject DBMS
#'
#' These methods are straight-forward implementations of the corresponding
#' generic functions.
#'
#' @param drv an object of class \code{BigObjectDriver}, or the character string
#'   "BigObject" or an \code{BigObjectConnection}.
#' @param username,password Username and password. If username omitted,
#'   defaults to the current user. If password is omitted, only users
#'   without a password can log in.
#' @param dbname string with the database name or NULL. If not NULL, the
#'   connection sets the default database to this value.
#' @param host string identifying the host machine running the BigObject server or
#'   NULL. If NULL or the string \code{"localhost"}, a connection to the local
#'   host is assumed.
#' @param port (optional) integer of the TCP/IP default port.
#' @param ... Unused, needed for compatibility with generic.
#' @import methods RMariaDB
#' @export
#' @examples
#' \dontrun{
#' # Connect to a BigObject database running locally
#' con <- dbConnect(RBigObject::BigObject(), dbname = "mydb")
#' # Connect to a remote database with username and password
#' con <- dbConnect(RBigObject::BigObject(), host = "mydb.mycompany.com",
#'   user = "abc", password = "def")
#' # But instead of supplying the username and password in code, it's usually
#'
#' # Always cleanup by disconnecting the database
#' dbDisconnect(con)
#' }
#'
#' @export
setMethod("dbConnect", "BigObjectDriver",
  function(drv, ...) {
    backend <- do.call("dbConnect", append(list(drv=RMariaDB::MariaDB()), list(...)))

    #ptr <- connection_create(host, username, password, dbname, port, unix.socket,
    #  client.flag, groups, default.file, ssl.key, ssl.cert, ssl.ca, ssl.capath,
    #  ssl.cipher)

    #info <- connection_info(ptr)

    con <- new("BigObjectConnection",
      backend = backend
    )

    con
  }
)

#' @param max.con DEPRECATED
#' @param fetch.default.rec DEPRECATED
#' @export
#' @import methods DBI
#' @rdname dbConnect-BigObjectDriver-method
#' @examples
#' # connect to a database and load some data
#' con <- dbConnect(RBigObject::BigObject(), dbname = "test")
#' dbWriteTable(con, "USArrests", datasets::USArrests, overwrite = TRUE)
#'
#' # query
#' rs <- dbSendQuery(con, "SELECT * FROM USArrests")
#' d1 <- dbFetch(rs, n = 10)      # extract data in chunks of 10 rows
#' dbHasCompleted(rs)
#' d2 <- dbFetch(rs, n = -1)      # extract all remaining data
#' dbHasCompleted(rs)
#' dbClearResult(rs)
#' dbListTables(con)
#'
#' # clean up
#' dbRemoveTable(con, "USArrests")
#' dbDisconnect(con)
BigObject <- function() {
  new("BigObjectDriver")
}

