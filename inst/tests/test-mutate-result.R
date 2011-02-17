context("Testing mutation of Results")

query <- session$query(db1$v1)
expect_error(query$execute(mutable = TRUE)$all())

result <- session$query(db1$i1)$execute(mutable = TRUE)$all()
expect_is(result, "Result")
expect_true(result$getOption("mutable"))

random <- sample(1:10000, 1)
result[1, ] <- random
expect_equal(result[1, ], random)
expect_equal(result$result[1, ], random)
result$flush()

result.df <- session$query(db1$i1)$execute()$all()
expect_is(result.df, "data.frame")
expect_true(random %in% result.df$i1)
