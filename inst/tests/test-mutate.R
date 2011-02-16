library(testthat)
library(databasr)

context("Testing mutation")

session <- Session$new("MySQL")

test <- introspectTable(session, "test")

query <- session$query(test$b)
expect_error(query$execute(mutable = TRUE)$getAll())

result <- session$query(test$a)$execute(mutable = TRUE)$getAll()
expect_is(result, "Result")
expect_true(result$isMutable())

random <- sample(1:100, 1)
result[1, ] <- random
expect_equal(result[1, ], random)
expect_equal(result$result[1, ], random)
result$flush()

result.df <- session$query(test$a)$execute()$getAll()
expect_is(result.df, "data.frame")
expect_true(random %in% result.df$a)

session$finish()
