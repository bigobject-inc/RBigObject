#' @include BigObjectConnection.R
#' @include BigObjectResult.R
NULL

#' Execute a SQL statement on a database connection.
#'
#' To retrieve results a chunk at a time, use \code{dbSendQuery},
#' \code{dbFetch}, then \code{dbClearResult}. Alternatively, if you want all the
#' results (and they'll fit in memory) use \code{dbGetQuery} which sends,
#' fetches and clears for you.
#'
#' \code{fetch()} will be deprecated in the near future; please use
#' \code{dbFetch()} instead.
#'
#' @param conn an \code{\linkS4class{BigObjectConnection}} object.
#' @param res A  \code{\linkS4class{BigObjectResult}} object.
#' @inheritParams DBI::sqlRownamesToColumn
#' @param n Number of rows to retrieve. Use -1 to retrieve all rows.
#' @param params A list of query parameters to be substituted into
#'   a parameterised query.
#' @param statement a character vector of length one specifying the SQL
#'   statement that should be executed.  Only a single SQL statment should be
#'   provided.
#' @param ... Unused. Needed for compatibility with generic.
#' @export
#' @examples
#' con <- dbConnect(RBigObject::BigObject(), dbname = "test")
#' dbWriteTable(con, "arrests", datasets::USArrests, overwrite = TRUE)
#'
#' # Run query to get results as dataframe
#' dbGetQuery(con, "SELECT * FROM arrests limit 3")
#'
#' # Send query to pull requests in batches
#' res <- dbSendQuery(con, "SELECT * FROM arrests")
#' data <- dbFetch(res, n = 2)
#' data
#' dbHasCompleted(res)
#'
#' dbClearResult(res)
#' dbRemoveTable(con, "arrests")
#' dbDisconnect(con)
#' @rdname query
setMethod("dbFetch", c("BigObjectResult", "numeric"),
  function(res, n = -1, ..., row.names = FALSE) {
    if (length(n) != 1) stopc("n must be scalar")
    if (n < -1) stopc("n must be nonnegative or -1")
    if (is.infinite(n)) n <- -1
    if (trunc(n) != n) stopc("n must be a whole number")

    stmt <- toupper(trimws(dbGetInfo(res@bak_res)$statement))
    df <- dbFetch(res@bak_res, n, row.names = row.names)
    if (nrow(df) == 0) {
      if (!grepl("^((REMOTE|CLUSTER)[[:space:]]+|)SELECT", stmt)) {
        warning("shall return nothing.")
      }
    }
    df
  }
)

#' @rdname query
#' @export
setMethod("dbFetch", c("BigObjectResult", "missing"),
  function(res, n, ...) {
    dbFetch(res, n=-1)
  }
)

#' @rdname query
#' @export
setMethod("dbGetQuery", c("BigObjectConnection", "character"),
  function(conn, statement, n = -1L, ...) {
    if (length(n) != 1) stopc("n must be scalar")
    if (n < -1) stopc("n must be nonnegative or -1")
    if (is.infinite(n)) n <- -1
    if (trunc(n) != n) stopc("n must be a whole number")

	rs <- dbSendQuery(conn, statement, ...)
    on.exit(dbClearResult(rs))

    dbFetch(rs, n = n, ...)
  }
)


#' @rdname query
#' @export
setMethod("dbSendQuery", c("BigObjectConnection", "character"),
  function(conn, statement, params = NULL, ...) {
	if (statement == "SELECT" || length(statement) > 1) {
      stop("You have an error in your SQL syntax;")
	}

    rss <- dbListResults(conn@backend)
    if (length(rss) > 0) {
      dbClearResult(rss[[1]])
      warning("found a result set and close it")
    }

	bak_res <- dbSendQuery(conn@backend, statement)

    rs <- new("BigObjectResult",
      sql = statement,
      bak_res = bak_res
    )

    #if (!is.null(params)) {
    #  dbBind(rs, params)
    #}

    rs
  }
)

#' @rdname query
#' @export
setMethod("dbBind", "BigObjectResult", function(res, params, ...) {
  #result_bind(res@ptr, params)
  # TODO: support this!
  dbBind(res@bak_res, params)
  TRUE
})

#' @rdname query
#' @export
setMethod("dbClearResult", "BigObjectResult", function(res, ...) {
  if (!dbIsValid(res@bak_res)) { 
    warning("Expired, result set already closed") 
    return(invisible(TRUE)) 
  }

  dbClearResult(res@bak_res)
  invisible(TRUE)
})

#' @rdname query
#' @export
setMethod("dbGetStatement", "BigObjectResult", function(res, ...) {
  res@sql
})

#' Database interface meta-data.
#'
#' See documentation of generics for more details.
#'
#' @param res An object of class \code{\linkS4class{BigObjectResult}}
#' @param ... Ignored. Needed for compatibility with generic
#' @examples
#' con <- dbConnect(RBigObject::BigObject(), dbname = "test")
#' dbWriteTable(con, "t1", datasets::USArrests, overwrite = TRUE)
#'
#' rs <- dbSendQuery(con, "SELECT * FROM t1 WHERE UrbanPop >= 80")
#' rs
#'
#' dbGetStatement(rs)
#' dbHasCompleted(rs)
#' dbColumnInfo(rs)
#'
#' dbFetch(rs)
#' rs
#'
#' dbClearResult(rs)
#' dbRemoveTable(con, "t1")
#' dbDisconnect(con)
#' @name result-meta
NULL

#' @export
#' @rdname result-meta
setMethod("dbColumnInfo", "BigObjectResult", function(res, ...) {
  dbColumnInfo(res@bak_res)
})

#' @export
#' @rdname result-meta
setMethod("dbListFields", c("BigObjectConnection", "character"), function(conn, name) {
  # NOTE: DESC will cause fatal error. avoid use it. 
  #dbGetQuery(conn, paste0("DESC ", dbQuoteIdentifier(conn, table)))[[1]]
  rs <- dbSendQuery(conn, paste0("SELECT * FROM ", name, " LIMIT 1"))
  x <- dbColumnInfo(rs)$name
  dbClearResult(rs)
  x
})

#' @export
#' @rdname result-meta
setMethod("dbGetRowsAffected", "BigObjectResult", function(res, ...) {
  dbGetRowsAffected(res@bak_res)
})

#' @export
#' @rdname result-meta
setMethod("dbGetRowCount", "BigObjectResult", function(res, ...) {
  dbGetRowCount(res@bak_res)
})

#' @export
#' @rdname result-meta
setMethod("dbHasCompleted", "BigObjectResult", function(res, ...) {
  dbHasCompleted(res@bak_res)
})

