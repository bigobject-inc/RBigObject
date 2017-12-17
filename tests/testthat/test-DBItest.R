if (identical(Sys.getenv("NOT_CRAN"), "true")) {
    test_driver_skip <- c("get_info_driver")
    test_connection_skip <- c(#"disconnect_formals", 
                              #"can_disconnect", 
                              #"disconnect_closed_connection", 
                              #"data_type_connection", 
                              "get_info_connection", 
                              "disconnect_invalid_connection", 
                              "cannot_forget_disconnect", 
                              NULL)
    test_result_skip <- c(".*invalid_connection", 
                          #"send_query_trivial", 
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
    test_sql_skip <- c(".*invalid_connection", 
                       "quote_string_roundtrip", 
                       "quote_string_na", 
                       "quote_string_na_is_null", 
                       "quote_identifier", 
                       #"quote_identifier_string", 
                       #"quote_identifier_special", 
                       "read_table_name",
                       "write_table_overwrite",
                       "write_table_append_incompatible",
                       "overwrite_table_missing", 
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

                       NULL)
    test_meta_skip <- c("bind_.*", 
                        #"is_valid_formals", 
                        #"is_valid_connection", 
                        #"is_valid_stale_connection", 
                        #"is_valid_result_query", 
                        #"is_valid_result_statement", 
                        #"has_completed_formals", 
                        #"has_completed_query", 
                        #"has_completed_statement", 
                        #"has_completed_error", 
                        #"has_completed_query_spec", 
                        #"get_statement_formals", 
                        #"get_statement_query", 
                        #"get_statement_statement", 
                        #"get_statement_error", 
                        "column_info", 
                        #"get_row_count_formals", 
                        "row_count_query", 
                        #"row_count_statement", 
                        #"get_row_count_error", 
                        #"get_rows_affected_formals", 
                        "rows_affected_statement", 
                        "rows_affected_query", 
                        #"get_rows_affected_error", 
                        "get_info_result", 

                        NULL)

    
    # Passed
    #DBItest::test_getting_started()
    #DBItest::test_driver(skip=test_driver_skip)
    #DBItest::test_connection(skip=test_connection_skip)
    #DBItest::test_result(skip=test_result_skip) 
    #DBItest::test_sql(skip=test_sql_skip)
    #DBItest::test_meta(skip=test_meta_skip)
    #DBItest::test_compliance()
    # Testing
    DBItest::test_stress(skip=c("simultaneous_connections"))
    # Scheduled for testing
    # No support
    #DBItest::test_transaction()



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
