context("Testing compilation of LIMIT and OFFSET")

statement.base <- prepareStatement("SELECT
  `%database`.`databasr_test_3`.`i1` AS `i1`, `%database`.`databasr_test_3`.`d1` AS `d1`
FROM
  `%database`.`databasr_test_3`
LIMIT %d
OFFSET %d;"
)


statement <- session$query(db3)[1:1]$SQL()
expect_equal(statement, sprintf(statement.base, 1, 0))

statement <- session$query(db3)[1:10]$SQL()
expect_equal(statement, sprintf(statement.base, 10, 0))

statement <- session$query(db3)[1:10000]$SQL()
expect_equal(statement, sprintf(statement.base, 10000, 0))
