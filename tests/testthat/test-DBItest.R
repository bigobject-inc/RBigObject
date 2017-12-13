if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    test_driver_skip <- c("get_info_driver")
    test_connection_skip <- c("get_info_connection")
    test_result_skip <- c("send_query_trivial", 
                          "send_query_only_one_result_set", 
                          "fetch_atomic",
                          "fetch_one_row",
                          "fetch_n_good_after_bad",
                          "fetch_multi_row_single_column",
                          "fetch_multi_row_multi_column",
                          "fetch_n_progressive",
                          "fetch_n_multi_row_inf",
                          "fetch_n_more_rows", 
                          "fetch_n_zero_rows", 
                          "fetch_n_premature_close", 
                          "fetch_row_names", 
                          "get_query_multi_row_single_column", 
                          "get_query_multi_row_multi_column", 
                          "get_query_n_multi_row_inf", 
                          "get_query_n_more_rows", 
                          "get_query_n_zero_rows", 
                          "get_query_n_incomplete", 
                          "get_query_atomic", 
                          "get_query_one_row", 
                          "get_query_good_after_bad_n",
                          "get_query_row_names",
                          "data_integer", 
                          "data_numeric", 
                          "data_logical", 
                          "data_character", 
                          "data_raw", 
                          "data_date", 
                          "data_date_current",
                          "data_time", 
                          "data_time_current", 
                          "data_timestamp", 
                          "data_timestamp_current", 
                          "data_null",
                          "data_date_typed", 
                          "data_date_current_typed", 
                          "data_timestamp_typed", 
                          "data_timestamp_current_typed", 
                          "data_64_bit_numeric", 
                          "data_64_bit_numeric_warning", 
                          "data_64_bit_lossless",

                          NULL)
    test_sql_skip <- c(
                       #"quote_string_formals", 
                       #"quote_string_return", 
                       #"quote_string_vectorized", 
                       #"quote_string_double", 
                       "quote_string_roundtrip", 
                       "quote_string_na", 
                       "quote_string_na_is_null", 
                       #"quote_identifier_formals", 
                       #"quote_identifier_return", 
                       #"quote_identifier_vectorized", 
                       "quote_identifier", 
                       "quote_identifier_string", 
                       "quote_identifier_special", 
                       #"read_table_formals", 
                       #"read_table", 
                       #"read_table_missing", 
                       #"read_table_empty", 
                       #"read_table_row_names_false", 
                       #"read_table_row_names_true_exists", 
                       #"read_table_row_names_true_missing", 
                       #"read_table_row_names_na_exists", 
                       #"read_table_row_names_na_missing", 
                       #"read_table_row_names_string_exists", 
                       #"read_table_row_names_string_missing", 
                       #"read_table_row_names_default", 
                       #"read_table_check_names", 
                       #"read_table_closed_connection", 
                       #"read_table_invalid_connection", 
                       #"read_table_error", 
                       #"read_table_name", 
                       #"write_table_formals", 
                       #"write_table_return", 
                       #"write_table_overwrite", 
                       #"write_table_append_incompatible", 
                       #"write_table_closed_connection", 
                       #"write_table_invalid_connection", 
                       #"write_table_error", 
                       #"write_table_name", 
                       #"overwrite_table", 
                       "overwrite_table_missing", 
                       #"append_table", 
                       #"append_table_new", 
                       #"temporary_table", 
                       #"table_visible_in_other_connection", 
                       "roundtrip_keywords", 
                       "roundtrip_quotes", 
                       "roundtrip_integer", 
                       "roundtrip_numeric", 
                       "roundtrip_numeric_special", 
                       "roundtrip_logical", 
                       "roundtrip_null", 
                       "roundtrip_64_bit_numeric", 
                       "roundtrip_64_bit_character", 
                       "roundtrip_character", 
                       "roundtrip_character_native", 
                       "roundtrip_character_empty", 
                       "roundtrip_factor", 
                       "roundtrip_raw", 
                       "roundtrip_blob", 
                       "roundtrip_date", 
                       "roundtrip_time", 
                       "roundtrip_timestamp", 
                       "roundtrip_mixed", 
                       "roundtrip_field_types", 
                       #"write_table_row_names_false", 
                       #"write_table_row_names_true_exists", 
                       #"write_table_row_names_true_missing", 
                       #"write_table_row_names_na_exists", 
                       #"write_table_row_names_na_missing", 
                       #"write_table_row_names_string_exists", 
                       #"write_table_row_names_string_missing", 
                       #"list_tables_formals", 
                       #"list_tables", 
                       #"list_tables_closed_connection", 
                       #"list_tables_invalid_connection", 
                       #"exists_table_formals", 
                       #"exists_table", 
                       #"exists_table_closed_connection", 
                       #"exists_table_invalid_connection", 
                       #"exists_table_error", 
                       #"exists_table_name", 
                       #"exists_table_list", 
                       "remove_table_formals", 
                       "remove_table_return", 
                       "remove_table_missing", 
                       "remove_table_closed_connection", 
                       "remove_table_invalid_connection", 
                       "remove_table_error", 
                       "remove_table_list", 
                       "remove_table_other_con", 
                       "remove_table_temporary", 
                       "remove_table_name", 
                       "list_fields", 
                       "list_fields_row_names", 

                       NULL)
    
    # Passed
    #DBItest::test_getting_started()
    #DBItest::test_driver(skip=test_driver_skip)
    #DBItest::test_connection(skip=test_connection_skip)
    #DBItest::test_result(skip=test_result_skip) 
    # Testing
    DBItest::test_sql(skip=test_sql_skip)
    # Scheduled for testing
    #DBItest::test_meta()
    #DBItest::test_transaction()
    #DBItest::test_compliance()
    #DBItest::test_stress()



# TODO: Use this as the final test case
#DBItest::test_all(skip=c(
#  # driver
#  "get_info_driver",                            # rstats-db/RSQLite#117
#
#  # connection
#  "get_info_connection",                        # rstats-db/RSQLite#117
#
#  # result
#  "data_logical",                               # not an error: cannot cast to logical
#  "data_raw",                                   # not an error: can't cast to blob type
#
#  # meta
#  "get_info_result",                            # rstats-db/DBI#55
#
#  NULL
#))

}
