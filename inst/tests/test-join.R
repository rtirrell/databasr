context("Testing compilation of JOIN")
statement <- prepareStatement("SELECT
  `%database`.`databasr_test_1`.`i1` AS `i1`, `%database`.`databasr_test_1`.`v1` AS `v1`
FROM
  `%database`.`databasr_test_1`
JOIN
  `%database`.`databasr_test_2`
ON
  `%database`.`databasr_test_2`.`i1` = `%database`.`databasr_test_1`.`i1`;"
)

q1 <- session$select(db1)$join(db2$i1 == db1$i1)
expect_equal(q1$SQL(), statement)
r1 <- q1$execute()$all()

statement <- prepareStatement("SELECT
  `%database`.`databasr_test_1`.`i1` AS `i1`, `%database`.`databasr_test_1`.`v1` AS `v1`
FROM
  `%database`.`databasr_test_1`
JOIN
  `%database`.`databasr_test_2`
USING (
  `i1`
);")

q2 <- session$select(db1)$join(db2$i1)
expect_equal(q2$SQL(), statement)
r2 <- q2$execute()$all()
expect_equal(r1, r2)

statement <- prepareStatement("SELECT
  `%database`.`databasr_test_1`.`i1` AS `i1`, `%database`.`databasr_test_1`.`v1` AS `v1`
FROM
  `%database`.`databasr_test_1`
NATURAL JOIN
  `%database`.`databasr_test_2`;")
q3 <- session$select(db1)$join(db2)
expect_equal(q3$SQL(), statement)