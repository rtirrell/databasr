context("Testing compilation of ORDER BY")

statements <- list(
	session$select(db1$i1)$order(-db1$i1)$sql(),
	session$select(db1$i1)$order(-db1$i1, +db1$v1)$sql()
)

compiled <- list(
	"",
	prepare_sql(", `%database`.`databasr_test_1`.`v1` ASC")
)

statement.base <- prepare_sql("SELECT
  `%database`.`databasr_test_1`.`i1` AS `i1`
FROM
  `%database`.`databasr_test_1`
ORDER BY
  `%database`.`databasr_test_1`.`i1` DESC%s;"
)

for (i in seq_along(statements)) {
	expect_equal(statements[[i]], sprintf(statement.base, compiled[[i]]))
}
