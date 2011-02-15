library(testthat)
library(databasr)

context("Testing compilation of LIMIT and OFFSET")

session <- Session$new("MySQL")

test <- introspectTable(session, "test")

statement.base <- "SELECT
\t`user_rpt`.`test`.`a` AS `a`, `user_rpt`.`test`.`b` AS `b`
FROM
\t`user_rpt`.`test`
LIMIT %d
OFFSET %d;"


statement <- session$query(test)[1:1]$SQL()
expect_equal(statement, sprintf(statement.base, 1, 0))

statement <- session$query(test)[1:10]$SQL()
expect_equal(statement, sprintf(statement.base, 10, 0))

statement <- session$query(test)[1:10000]$SQL()
cat('\n', statement, '\n')
expect_equal(statement, sprintf(statement.base, 10000, 0))
cat('\n', sprintf(statement.base, 10000, 0), '\n')

session$finish()
