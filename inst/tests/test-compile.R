context("Testing compilation")

statement.base <- prepareStatement("SELECT
  `%database`.`databasr_test_1`.`%s` AS `%s`
FROM
  `%database`.`databasr_test_1`;"
)

expressions <- list(
	db1$i1,
	db1$v1
)

compiled <- list(
	list("i1", "i1"),
	list("v1", "v1")
)

for (i in seq_along(expressions)) {
	statement <- session$select(expressions[[i]])$SQL()
	expect_equal(statement, do.call(sprintf, c(statement.base, compiled[[i]])))
}

#statement <- session$select(db1$i1)$SQL()
#expect_equal(statement, sprintf(statement.base, "i1", "i1"))
#
#statement <- session$select(test$v1)$SQL()
#expect_equal(statement, sprintf(statement.base, "v1", "v1"))
