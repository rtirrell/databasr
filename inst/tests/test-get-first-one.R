query <- session$query(db1)[1:1]
result.1 <- query$one()
expect_equal(nrow(result.1), 1)
result.2 <- query$execute()$one()
expect_equal(nrow(result.1), nrow(result.2))

result <- query$execute()
result$one()
expect_equal(result$getAffectedRowCount(), 1)
expect_equal(result$getFetchedRowCount(), 2)

query <- session$query(db4)
result.1 <- query$first()
expect_equal(nrow(result.1), 1)
result.2 <- query$execute()$first()
expect_equal(nrow(result.1), nrow(result.2))

result <- query$execute()
result$first()
expect_equal(result$getAffectedRowCount(), 1)
expect_equal(result$getFetchedRowCount(), 1)
