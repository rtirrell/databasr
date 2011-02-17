context("Testing compilation of IN")

statement.base <- prepareStatement("SELECT
  `%database`.`databasr_test_1`.`i1` AS `i1`
FROM
  `%database`.`databasr_test_1`
WHERE
  `%database`.`databasr_test_1`.`i1` IN (%s);"
)


expressions <- list(
	db1$i1 %in% 1, 
	db1$i1 %in% c(1, 2), 
	db1$i1 %in% list(1, c(1, 2)), 
	db1$i1 %in% list(c(1, 2), c(3, 4))
)
compiled <- list(
	"1", "1, 2", "1, (1, 2)", "(1, 2), (3, 4)"
)

for (i in seq_along(expressions)) {
	q <- session$query(db1$i1)$where(expressions[[i]])
	expect_equal(q$SQL(), sprintf(statement.base, compiled[[i]]))
}
statement.base <- prepareStatement("SELECT
  `%database`.`databasr_test_2`.`v1` AS `v1`, `%database`.`databasr_test_2`.`v2` AS `v2`
FROM
  `%database`.`databasr_test_2`
WHERE
  (`%database`.`databasr_test_2`.`v1`, `%database`.`databasr_test_2`.`v2`) IN (%s);"
)

# Also test pairs of different types.
expressions <- list(
	tuple(db2$v1, db2$v2) %in% tuple(c('hello', 'goodbye')),
	tuple(db2$v1, db2$v2) %in% list(list(1, '1'), list('2', 2)),
	tuple(db2$v1, db2$v2) %in% tuple(list('3', 3), list('p', 'q'))
)
compiled <- list(
	"('hello', 'goodbye')",
	"(1, '1'), ('2', 2)",
	"('3', 3), ('p', 'q')"
)
for (i in seq_along(expressions)) {
	q <- session$query(db2$v1, db2$v2)$where(expressions[[i]])
	expect_equal(q$SQL(), sprintf(statement.base, compiled[[i]]))
}
	
