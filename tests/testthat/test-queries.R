context("queries")

# Can't test this in a generic fashion
#test_that("setting parameter query is always complete", {
#  conn <- dbConnect(RBigObject::BigObject(), dbname="bigobject", host="192.168.1.64", port=23306)
#  rs <- dbSendQuery(conn, 'SET time_zone = "+00:00"')
#
#  expect_true(dbHasCompleted(rs))
#
#  dbClearResult(rs)
#  dbDisconnect(conn)
#})
