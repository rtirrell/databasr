library(testthat)
library(databasr)

context("Testing compilation")

session <- Session$new("MySQL")
test <- introspectTable(session, "test")
statement.base <- "SELECT
  `user_rpt`.`test`.`%s` AS `%s`
FROM
  `user_rpt`.`test`;"

statement <- session$query(test$a)$SQL()
expect_equal(statement, sprintf(statement.base, "a", "a"))

statement <- session$query(test$b)$SQL()
expect_equal(statement, sprintf(statement.base, "b", "b"))

session$finish()