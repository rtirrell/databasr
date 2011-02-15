library(testthat)
library(databasr)

context("Testing compilation of WHERE")

session <- Session$new("MySQL")

test <- introspectTable(session, "test")

statement.base <- "SELECT
  `user_rpt`.`test`.`a` AS `a`, `user_rpt`.`test`.`b` AS `b`
FROM
  `user_rpt`.`test`
WHERE
  `user_rpt`.`test`.`a` = %d AND `user_rpt`.`test`.`b` = '%s';"

statement <- session$query(test)$where(test$a == 0 & test$b == '')$SQL()
expect_equal(statement, sprintf(statement.base, 0, ''))

statement <- session$query(test)$where(test$a == 10 & test$b == 'ten')$SQL()
expect_equal(statement, sprintf(statement.base, 10, 'ten'))

statement <- session$query(test)$where(test$a == -30 & test$b == '\\n')$SQL()
expect_equal(statement, sprintf(statement.base, -30, '\\n'))

session$finish()
