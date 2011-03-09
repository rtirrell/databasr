context("Testing compilation of LIMIT and OFFSET")

statement.base <- prepare_sql("SELECT
  `%database`.`databasr_test_3`.`i1` AS `i1`, `%database`.`databasr_test_3`.`d1` AS `d1`
FROM
  `%database`.`databasr_test_3`
LIMIT %d
OFFSET %d;"
)


statement <- session$select(db3)[1:1]$sql()
expect_equal(statement, sprintf(statement.base, 1, 0))

statement <- session$select(db3)[1:10]$sql()
expect_equal(statement, sprintf(statement.base, 10, 0))

statement <- session$select(db3)[1:10000]$sql()
expect_equal(statement, sprintf(statement.base, 10000, 0))
