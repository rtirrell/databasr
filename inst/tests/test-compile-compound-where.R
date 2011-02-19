context("Testing compilation of compound WHERE")

expressions <- list(
	db1$i1 == 2 | tuple(db2$i1 == 2 | db2$v1 == 4),
	tuple(db1$i1) == 2 | tuple(db2$i1 == 2 & db2$v1 == 4)
)

compiled <- list(
	list("", "", "OR"),
	list("(", ")", "AND")
)

statement.base <- prepareStatement("SELECT
  `%database`.`databasr_test_1`.`i1` AS `i1`
FROM
  `%database`.`databasr_test_1`, `%database`.`databasr_test_2`
WHERE
  %s`%database`.`databasr_test_1`.`i1`%s = 2 OR (`%database`.`databasr_test_2`.`i1` = 2 %s `%database`.`databasr_test_2`.`v1` = 4);"
)

for (i in seq_along(expressions)) {
	statement <- session$query(db1$i1)$where(expressions[[i]])$SQL()
	expect_equal(statement, do.call(sprintf, c(statement.base, compiled[[i]])))
}