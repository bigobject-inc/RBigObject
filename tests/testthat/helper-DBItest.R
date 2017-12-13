DBItest::make_context(
  BigObject(),
  list(dbname = "bigobject", username = "root", password = "aaaa6666", port=23306),
  tweaks = DBItest::tweaks(
    constructor_relax_args = TRUE,
	omit_blob_tests = TRUE, 
    temporary_tables = FALSE, # BO's temporary table is different to MySQL's. https://dev.mysql.com/doc/refman/5.7/en/create-temporary-table.html
    strict_identifier = TRUE, 
    placeholder_pattern = "?"
  ),
  name = "RBigObject")
