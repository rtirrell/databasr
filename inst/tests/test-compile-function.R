context("Testing compilation of functions")

statement.base <- prepareStatement("SELECT
  `%database`.`databasr_test_4`.`i1` AS `i1`, %s(`%database`.`databasr_test_4`.`d1`) AS `%s`
FROM
  `%database`.`databasr_test_4`;"
)

expressions <- list(
	length(db4$d1),
	min(db4$d1),
	max(db4$d1),
	unique(db4$d1),
	tolower(db4$d1),
	toupper(db4$d1)
)
compiled <- list(
	list("COUNT", "count_d1"),
	list("MIN", "min_d1"),
	list("MAX", "max_d1"),
	list("DISTINCT", "distinct_d1"),
	list("LOWER", "lower_d1"),
	list("UPPER", "upper_d1")
)

for (i in seq_along(expressions)) {
	statement <- session$query(db4$i1, expressions[[i]])$SQL()
	expect_equal(statement, do.call(sprintf, c(statement.base, compiled[[i]])))
}
	

#statement <- session$query(db4$i1, length(db4$d1))$SQL()
#expect_equal(statement, sprintf(statement.base, "COUNT", "count_d1"))
#
#statement <- session$query(db4$i1, min(db4$d1))$SQL()
#expect_equal(statement, sprintf(statement.base, "MIN", "min_d1"))
#
#statement <- session$query(db4$i1, max(db4$d1))$SQL()
#expect_equal(statement, sprintf(statement.base, "MAX", "max_d1"))
#
#statement <- session$query(db4$i1, unique(db4$d1))$SQL()
#expect_equal(statement, sprintf(statement.base, "DISTINCT", "distinct_d1"))
#
#statement <- session$query(db4$i1, tolower(db4$d1))$SQL()
#expect_equal(statement, sprintf(statement.base, "LOWER", "lower_d1"))
#
#statement <- session$query(db4$i1, toupper(db4$d1))$SQL()
#expect_equal(statement, sprintf(statement.base, "UPPER", "upper_d1"))

statement.base <- prepareStatement("SELECT
  CONCAT(`%database`.`databasr_test_4`.`i1`, `%database`.`databasr_test_4`.`d1`) AS `%s`
FROM
  `%database`.`databasr_test_4`;"
)

#statement <- session$query(c(db4$i1, db4$d1))$SQL()
#expect_equal(statement, sprintf(statement.base, 'concat_1'))
#statement <- session$query(c(db4$i1, db4$d1)$as('ab_count'))$SQL()
#expect_equal(statement, sprintf(statement.base, 'ab_count'))
