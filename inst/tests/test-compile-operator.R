context("Testing compilation of operator")


# Test that we are not breaking our queries by making modifications at compile time. Previously,
# NegatableBinaryOperators would break as follows:
#   first time: compiles fine, but *we change the operator that is set*
#   second time: compiles fine (but the SQL is broken, as it's missing an operator), 
#			but the operator doesn't match in our switch expression and becomes	NULL.
#		third time: dies with NULL operator.
statements <- list(
	session$query(db1$i1)$where(db1$i1 != 1),
	session$query(db1$i1, db2$i1)$from(db1)$where(db2$i1 != 1),
	session$query(db4$v1, db2$v1)$join(db3)$where(db4$v1 != (db2$v2 == db3$i1))
)
	
for (statement in statements) {
	compiled <- statement$SQL()
	all.compiled <- replicate(5, statement$SQL())
	expect_true(all(all.compiled == compiled))
}

expressions <- list(
	db3$i1 != NA,
	db3$i1 == NA,
	db3$d1 != NA,
	db3$d1 == NA
)

compiled <- list(
	list("i1", "IS NOT NULL"),
	list("i1", "IS NULL"),
	list("d1", "IS NOT NULL"),
	list("d1", "IS NULL")
)

statement.base <- prepareStatement("SELECT
  `%database`.`databasr_test_3`.`i1` AS `i1`, `%database`.`databasr_test_3`.`d1` AS `d1`
FROM
  `%database`.`databasr_test_3`
WHERE
  `%database`.`databasr_test_3`.`%s` %s;"
)

for (i in seq_along(expressions)) {
	statement <- session$query(db3)$where(expressions[[i]])$SQL()
	expect_equal(statement, do.call(sprintf, c(statement.base, compiled[[i]])))
}