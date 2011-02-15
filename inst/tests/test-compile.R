library(testthat)
library(databasr)

context("Testing compilation")

session <- Session$new("MySQL")
test <- introspectTable(session, "test")

statement <- session$query(test$a)$SQL()
expect_equal(statement, "SELECT\n\t`user_rpt`.`test`.`a` AS `a`\nFROM\n\t`user_rpt`.`test`;")

statement <- session$query(test$b)$SQL()
expect_equal(statement, "SELECT\n\t`user_rpt`.`test`.`b` AS `b`\nFROM\n\t`user_rpt`.`test`;")

session$finish()