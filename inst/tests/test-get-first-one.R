query <- session$select(db1)[1:1]
result.1 <- query$one()
expect_equal(nrow(result.1), 1)
result.2 <- query$execute()$one()
expect_equal(nrow(result.1), nrow(result.2))

result <- query$execute()
result$one()
expect_equal(result$get_affected_count(), 1)

query <- session$select(db4)
result.1 <- query$first()
expect_equal(nrow(result.1), 1)
result.2 <- query$execute()$first()
expect_equal(nrow(result.1), nrow(result.2))

result <- query$execute()
result$first()
expect_equal(result$get_affected_count(), 1)