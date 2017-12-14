#' @include BigObjectConnection.R
NULL

#' Read and write BigObject tables.
#'
#' These functions mimic their R counterpart \code{get}, \code{assign},
#' \code{exists}, \code{remove}, and \code{ls}.
#'
#' @return A data.frame in the case of \code{dbReadTable}; otherwise a logical
#' indicating whether the operation was successful.
#' @note Note that the data.frame returned by \code{dbReadTable} only has
#' primitive data, e.g., it does not coerce character data to factors.
#'
#' @param conn a \code{\linkS4class{BigObjectConnection}} object, produced by
#'   \code{\link[DBI]{dbConnect}}
#' @param name a character string specifying a table name.
#' @param check.names If \code{TRUE}, the default, column names will be
#'   converted to valid R identifiers.
#' @inheritParams DBI::sqlRownamesToColumn
#' @param ... Unused, needed for compatiblity with generic.
#' @examples
#' con <- dbConnect(RBigObject::BigObject(), dbname = "test")
#'
#' # By default, row names are written in a column to row_names, and
#' # automatically read back into the row.names()
#' dbWriteTable(con, "mtcars", mtcars[1:5, ], temporary = TRUE)
#' dbReadTable(con, "mtcars")
#' dbReadTable(con, "mtcars", row.names = NULL)
#' @name bigobject-tables
NULL

#' @export
#' @rdname bigobject-tables
setMethod("dbReadTable", c("BigObjectConnection", "character"),
  function(conn, name, ..., row.names = FALSE, check.names = TRUE) {
    row.names <- compatRowNames(row.names)

    if ((!is.logical(row.names) && !is.character(row.names)) || length(row.names) != 1L)  {
      stopc("`row.names` must be a logical scalar or a string")
    }

    if (!is.logical(check.names) || length(check.names) != 1L)  {
      stopc("`check.names` must be a logical scalar")
    }

    name <- dbQuoteIdentifier(conn, name)

    out <- dbGetQuery(conn, paste("SELECT * FROM ", name),
      row.names = row.names)

    if (check.names) {
      names(out) <- make.names(names(out), unique = TRUE)
    }

    out

  }
)

#' @inheritParams DBI::sqlRownamesToColumn
#' @param overwrite a logical specifying whether to overwrite an existing table
#'   or not. Its default is \code{FALSE}. (See the BUGS section below)
#' @param append a logical specifying whether to append to an existing table
#'   in the DBMS.  If appending, then the table (or temporary table)
#'   must exist, otherwise an error is reported. Its default is \code{FALSE}.
#' @param value A data frame.
#' @param field.types Optional, overrides default choices of field types,
#'   derived from the classes of the columns in the data frame.
#' @param temporary If \code{TRUE}, creates a temporary table that expires
#'   when the connection is closed.
#' @param allow.keywords DEPRECATED.
#' @export
#' @rdname bigobject-tables
setMethod("dbWriteTable", c("BigObjectConnection", "character", "data.frame"),
  function(conn, name, value, field.types = NULL, row.names = NA,
           overwrite = FALSE, append = FALSE, ..., allow.keywords = FALSE,
           temporary = FALSE) {
    
    row.names <- compatRowNames(row.names)

    if ((!is.logical(row.names) && !is.character(row.names)) || length(row.names) != 1L)  {
      stopc("`row.names` must be a logical scalar or a string")
    }
    if (!is.logical(overwrite) || length(overwrite) != 1L || is.na(overwrite))  {
      stopc("`overwrite` must be a logical scalar")
    }
    if (!is.logical(append) || length(append) != 1L || is.na(append))  {
      stopc("`append` must be a logical scalar")
    }
    if (!is.logical(temporary) || length(temporary) != 1L)  {
      stopc("`temporary` must be a logical scalar")
    }

    if (overwrite && append) {
      stopc("overwrite and append cannot both be TRUE")
    }
    if (append && !is.null(field.types)) {
      stopc("Cannot specify field.types with append = TRUE")
    }

    if (!missing(allow.keywords)) {
      warning("allow.keywords is deprecated.")
    }


    found <- dbExistsTable(conn, name)
    if (found && !overwrite && !append) {
      stop("Table ", name, " exists in database, and both overwrite and",
        " append are FALSE", call. = FALSE)
    }
    if (found && overwrite) {
      dbRemoveTable(conn, name)
    }

    if (!found || overwrite) {
      sql <- sqlCreateTable(
        conn,
        name,
        if (is.null(field.types)) value else field.types,
        row.names = row.names,
        temporary = temporary
      )
      dbExecute(conn, sql)
    }

    if (nrow(value) > 0) {
      values <- sqlData(conn, value[, , drop = FALSE], row.names)

      name <- dbQuoteIdentifier(conn, name)
      fields <- dbQuoteIdentifier(conn, names(values))

      write.table.to.con(conn, values, name, fields)

    }

    invisible(TRUE)
  }
)

setMethod("sqlData", "BigObjectConnection", function(con, value, row.names = NA, ...) {
  value <- sqlRownamesToColumn(value, row.names)

  # Convert factors to strings
  is_factor <- vapply(value, is.factor, logical(1))
  value[is_factor] <- lapply(value[is_factor], as.character)

  # Ensure all in utf-8
  is_char <- vapply(value, is.character, logical(1))
  value[is_char] <- lapply(value[is_char], enc2utf8)

  value
})

write.table.to.con <- function(con, tab, tab.name, col.name) {
  partition <- sort(unique(c(seq.int(0, nrow(tab), by=1000), nrow(tab))))
  # use sapply if without parrallel
  cmds <- sapply(c(2:length(partition)), function(i) {
  #cmds <- parSapply(cl, c(2:length(partition)), function(i) {
    part.idx <- seq.int(partition[i-1] + 1, partition[i])
    insert.values <- unlist(apply(as.data.frame(tab[part.idx,]), 1, function(x){
      x.strs <- unlist(lapply(x, function(x) {
        if(is.character(x)) {
          outstr <- paste0("`", gsub("`", "\\\\`", gsub("\\\\", "\\\\\\\\", x)), "`")
        } else {
          outstr <- format(x)
        }
        #cat(paste0(outstr, "\n"))
        outstr 
      }), use.names=FALSE)
      str <- paste0("(", paste(x.strs, sep="", collapse=","), ")")
    }), recursive=FALSE, use.names=FALSE)
    cmd <- paste0("INSERT INTO ", tab.name, " (", paste0(col.name, collapse = ", "), ")", " VALUES ", paste(insert.values, collapse=","))
    #cat(paste0(cmd, "\n"))
    dbExecute(con, cmd)
  })
  # For parallel 
  #for (cmd in cmds) {
  #  dbExecute(con, cmd)
  #}
  TRUE
}


#' @export
#' @rdname bigobject-tables
setMethod("dbListTables", "BigObjectConnection", function(conn, ...) {
  dbGetQuery(conn, "SHOW TABLES")[[1]]
})

#' @export
#' @rdname bigobject-tables
setMethod("dbExistsTable", c("BigObjectConnection", "character"),
  function(conn, name, ...) {
    stopifnot(length(name) == 1L)
    if (!dbIsValid(conn)) {
      stopc("Invalid connection")
    }
    tryCatch({
      dbGetQuery(conn, paste0(
        "SELECT NULL FROM ", dbQuoteIdentifier(conn, name), " LIMIT 0"
      ))
      TRUE
    }, error = function(...) {
      FALSE
    })
  }
)

#' @export
#' @rdname bigobject-tables
setMethod("dbRemoveTable", c("BigObjectConnection", "character"),
  function(conn, name, ...){
    stopifnot(length(name) == 1L)
    if (!dbIsValid(conn)) {
      stopc("Invalid connection")
    }
    name <- dbQuoteIdentifier(conn, name)
    # Executing DROP TABLE ... will raise an exception in Rcpp.  
    #   Rcpp::exception in result_create(conn@ptr, statement): Object doesn't exist: iris [11]
    # The table is deleted even though there is an exception. Therefore we catch the exception and ignore it. 
    # The bug is not found in MariaDB. The packets in BigObject and MariaDB are the same. 
    # It's a workaround. 
    tryCatch({
      dbExecute(conn, paste0("DROP TABLE ", name))
    }, error = function(e){
    })
    invisible(TRUE)
  }
)

#' Determine the SQL Data Type of an S object
#'
#' This method is a straight-forward implementation of the corresponding
#' generic function.
#'
#' @param dbObj A \code{BigObjectDriver} or \code{BigObjectConnection}.
#' @param obj R/S-Plus object whose SQL type we want to determine.
#' @param \dots any other parameters that individual methods may need.
#' @export
#' @rdname dbDataType
#' @examples
#' dbDataType(RBigObject::BigObject(), "a")
#' dbDataType(RBigObject::BigObject(), 1:3)
#' dbDataType(RBigObject::BigObject(), 2.5)
setMethod("dbDataType", "BigObjectConnection", function(dbObj, obj, ...) {
  dbDataType(BigObject(), obj, ...)
})

#' @export
#' @rdname dbDataType
setMethod("dbDataType", "BigObjectDriver", function(dbObj, obj, ...) {
  if (is.factor(obj)) return("STRING(255)")
  if (inherits(obj, "POSIXct")) return("DATETIME64")
  if (inherits(obj, "Date")) return("DATE32")
  if (is.data.frame(obj)) return(unlist(lapply(obj, function(x) {dbDataType(dbObj, x)})))

  switch(typeof(obj),
    logical = "INT8",
    integer = "INT32",
    double = "DOUBLE",
    character = "STRING(255)",
    list = "VARSTRING(2048)",
    stop("Unsupported type", call. = FALSE)
  )
})
