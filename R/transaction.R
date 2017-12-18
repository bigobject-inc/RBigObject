#' @export
#' @rdname transactions
setMethod("dbBegin", "BigObjectConnection", function(conn, ...) {
  stopc("BigObject doesn't support begin")
  invisible(FALSE)
})

#' @export
#' @rdname transactions
setMethod("dbCommit", "BigObjectConnection", function(conn, ...) {
  stopc("BigObject doesn't support commit")
  invisible(FALSE)
})

#' @export
#' @rdname transactions
setMethod("dbRollback", "BigObjectConnection", function(conn, ...) {
  stopc("BigObject doesn't support rollback")
  invisible(FALSE)
})

