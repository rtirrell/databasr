context('Testing mutation of Result')

query <- session$select(db1$v1)
expect_error(query$execute(mutable = TRUE)$all())

result <- session$select(db1$i1)$execute(mutable = TRUE)$all()
expect_is(result, 'Result')
expect_true(result$get_option('mutable'))

random <- sample(1:10 * 100000, 1)
result[1, ] <- random
expect_equal(result$i1[1], random)
expect_equal(result$result$i1[1], random)
result$flush()

result.df <- session$select(db1$i1)$execute()$all()
expect_is(result.df, 'data.frame')
expect_true(random %in% result.df$i1)
