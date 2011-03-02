context("Testing compilation of functions")

statement.base <- prepareSQL("SELECT
  `%database`.`databasr_test_4`.`i1` AS `i1`, %s`%database`.`databasr_test_4`.`d1`%s AS `%s`
FROM
  `%database`.`databasr_test_4`;"
)

expressions <- list(
	length(db4$d1),
	min(db4$d1),
	max(db4$d1),
	unique(db4$d1),
	tolower(db4$d1),
	toupper(db4$d1),
	concat(db4$d1, "hello")
)
compiled <- list(
	list("COUNT(", ")", "count_d1"),
	list("MIN(", ")", "min_d1"),
	list("MAX(", ")", "max_d1"),
	list("DISTINCT ", "", "distinct_d1"),
	list("LOWER(", ")", "lower_d1"),
	list("UPPER(", ")", "upper_d1"),
	list("CONCAT(", ", 'hello')", "concat_d1")
)

for (i in seq_along(expressions)) {
	statement <- session$select(db4$i1, expressions[[i]])$SQL()
	expect_equal(statement, do.call(sprintf, c(statement.base, compiled[[i]])))
}
	

statement.base <- prepareSQL("SELECT
  CONCAT(`%database`.`databasr_test_4`.`i1`, `%database`.`databasr_test_4`.`d1`) AS `%s`
FROM
  `%database`.`databasr_test_4`;"
)

statement <- session$select(concat(db4$i1, db4$d1))$SQL()
expect_equal(statement, sprintf(statement.base, "concat_1"))
statement <- session$select(concat(db4$i1, db4$d1)$as("concat_id"))$SQL()
expect_equal(statement, sprintf(statement.base, "concat_id"))
