context("Testing compliation of alias")

expressions <- list(
	list(db1$i1, db2$i1),
	list(db1$v1, db2$v1),
	list(db1$v1, db2$i1)
)

compiled <- list(
	list("i1", "databasr_test_1_i1", "i1", "databasr_test_2_i1"),
	list("v1", "databasr_test_1_v1", "v1", "databasr_test_2_v1"),
	list("v1", "v1", "i1", "i1")
)

statement.base <- prepareStatement("SELECT
  `%database`.`databasr_test_1`.`%s` AS `%s`, `%database`.`databasr_test_2`.`%s` AS `%s`
FROM
  `%database`.`databasr_test_1`, `%database`.`databasr_test_2`;"
)

for (i in seq_along(expressions)) {
	statement <- do.call(session$select, expressions[[i]])$SQL()
	expect_equal(statement, do.call(sprintf, c(statement.base, compiled[[i]])))
}