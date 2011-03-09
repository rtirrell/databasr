context('Testing compilation and behavior of log functions')

result.1 <- session$select(db1$i1)$all()
result.2 <- session$select(log(db1$i1))$all()

expect_equal(log(result.1$i1), result.2$log_i1)

result.1 <- session$select(db1$i1)$all()
result.2 <- session$select(log2(db1$i1))$all()

expect_equal(log2(result.1$i1), result.2$log2_i1)

result.1 <- session$select(db1$i1)$all()
result.2 <- session$select(log10(db1$i1))$all()

expect_equal(log10(result.1$i1), result.2$log10_i1)
