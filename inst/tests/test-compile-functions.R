library(testthat)
library(databasr)

context("Testing function compilation")

session <- Session$new("MySQL")

test <- introspectTable(session, "test")

statement.base <- "SELECT
\t`user_rpt`.`test`.`a` AS `a`, %s(`user_rpt`.`test`.`b`) AS `%s`
FROM
\t`user_rpt`.`test`;"

statement <- session$query(test$a, length(test$b))$SQL()
expect_equal(statement, sprintf(statement.base, "COUNT", "count_b"))

statement <- session$query(test$a, min(test$b))$SQL()
expect_equal(statement, sprintf(statement.base, "MIN", "min_b"))

statement <- session$query(test$a, max(test$b))$SQL()
expect_equal(statement, sprintf(statement.base, "MAX", "max_b"))

statement <- session$query(test$a, unique(test$b))$SQL()
expect_equal(statement, sprintf(statement.base, "DISTINCT", "distinct_b"))

statement <- session$query(test$a, tolower(test$b))$SQL()
expect_equal(statement, sprintf(statement.base, "LOWER", "lower_b"))

statement <- session$query(test$a, toupper(test$b))$SQL()
expect_equal(statement, sprintf(statement.base, "UPPER", "upper_b"))

statement.base <- "SELECT
\tCONCAT(`user_rpt`.`test`.`a`, `user_rpt`.`test`.`b`) AS `%s`
FROM
\t`user_rpt`.`test`;"
#statement <- session$query(c(test$a, test$b))$SQL()
#expect_equal(statement, sprintf(statement.base, 'concat_1'))
#statement <- session$query(c(test$a, test$b)$as('ab_count'))$SQL()
#expect_equal(statement, sprintf(statement.base, 'ab_count'))

session$finish()