library(testthat)
library(databasr)

context("Testing compilation of JOIN")
session <- Session$new("MySQL")
test <- introspectTable(session, "test")
el.snps <- introspectTable(session, "el_snps")

statement <- session$query(test)$join(el.snps$position != test$a)$SQL()
expect_equal(statement, "SELECT
  `user_rpt`.`test`.`a` AS `a`, `user_rpt`.`test`.`b` AS `b`
FROM
  `user_rpt`.`test`
JOIN
  `user_rpt`.`el_snps`
ON
  `user_rpt`.`el_snps`.`position` != `user_rpt`.`test`.`a`;"
)