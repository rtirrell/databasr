context("Testing compilation of WHERE")

statement.base <- prepareStatement("SELECT
  `%database`.`databasr_test_2`.`i1` AS `i1`, `%database`.`databasr_test_2`.`v1` AS `v1`, `%database`.`databasr_test_2`.`v2` AS `v2`
FROM
  `%database`.`databasr_test_2`
WHERE
  `%database`.`databasr_test_2`.`i1` = %d AND `%database`.`databasr_test_2`.`v2` = '%s';"
)

expressions <- list(
	db2$i1 == 0 & db2$v2 == "",
	db2$i1 == 10 & db2$v2 == "ten",
	db2$i1 == -30 & db2$v2 == "\\n"
)
compiled <- list(
	list(0, ""),
	list(10, "ten"),
	list(-30, "\\n")
)

for (i in seq_along(expressions)) {
	statement <- session$select(db2)$where(expressions[[i]])$SQL()
	expect_equal(statement, do.call(sprintf, c(statement.base, compiled[[i]])))
}
	
#statement <- session$select(test)$where(test$a == 0 & test$b == '')$SQL()
#expect_equal(statement, sprintf(statement.base, 0, ''))
#
#statement <- session$select(test)$where(test$a == 10 & test$b == 'ten')$SQL()
#expect_equal(statement, sprintf(statement.base, 10, 'ten'))
#
#statement <- session$select(test)$where(test$a == -30 & test$b == '\\n')$SQL()
#expect_equal(statement, sprintf(statement.base, -30, '\\n'))
