#DBItest::make_context(BigObject(), list(port=23306, dbname="bigobject"))
#DBItest::make_context(MariaDB(), list(port=3308, dbname="gama", password="aaaa6666"))
DBItest::make_context(MySQL(), list(port=3308, dbname="gama", password="aaaa6666"))
DBItest::test_getting_started()
#DBItest::test_driver()
#DBItest::test_connection()
#DBItest::test_result()
DBItest::test_sql()
DBItest::test_meta()
DBItest::test_compliance()

#context("DBItest")
#
#test_that("multiplication works", {
#  expect_equal(2 * 2, 4)
#})
