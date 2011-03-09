context('Testing compilation of multiple WHEREs')

statement <- prepare_sql("SELECT
  `%database`.`databasr_test_1`.`v1` AS `v1`
FROM
  `%database`.`databasr_test_1`
WHERE
  `%database`.`databasr_test_1`.`v1` = 4 AND `%database`.`databasr_test_2`.`v2` IS NOT NULL;"
)
query <- session$select(db1$v1)$where(db1$v1 == 4)$where(db2$v2 != NA)
expect_equal(query$sql(), statement)

statement <- prepare_sql("SELECT
  `%database`.`databasr_test_1`.`v1` AS `v1`
FROM
  `%database`.`databasr_test_1`
WHERE
  `%database`.`databasr_test_1`.`v1` = 'test' AND `%database`.`databasr_test_1`.`i1` < `%database`.`databasr_test_1`.`i1`;"
)
query <- session$select(db1$v1)$where(db1$v1 == 'test')$where(db1$i1 < db1$i1)
expect_equal(query$sql(), statement)