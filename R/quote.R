#' @include BigObjectConnection.R
NULL

#' Quote BigObject strings and identifiers.
#'
#' In BigObject, identifiers are enclosed in backticks, e.g. \code{`x`}.
#'
#' @keywords internal
#' @name bigobject-quoting
#' @examples
#' con <- dbConnect(RBigObject::BigObject())
#' dbQuoteIdentifier(con, c("a b", "a`b"))
#' dbQuoteString(con, c("a b", "a'b"))
#' dbDisconnect(con)
NULL

#' @rdname bigobject-quoting
#' @export
setMethod("dbQuoteIdentifier", c("BigObjectConnection", "character"),
  function(conn, x, ...) {
    x <- gsub('`', '``', x, fixed = TRUE)
    SQL(paste('`', x, '`', sep = ""))
  }
)

#' @rdname bigobject-quoting
#' @export
setMethod("dbQuoteString", c("BigObjectConnection", "character"),
  function(conn, x, ...) {
	dbQuoteString(conn@backend, x)
  }
)
