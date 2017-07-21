DBItest::make_context(
  BigObject(),
  list(dbname = "bigobject", username = "root", password = "aaaa6666", port=23306),
  tweaks = DBItest::tweaks(
    constructor_relax_args = TRUE,
	omit_blob_tests = TRUE, 
    placeholder_pattern = "?"
  ),
  name = "RBigObject")
