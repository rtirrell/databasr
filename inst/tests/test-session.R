library(testthat)
library(databasr)

context("Testing Session")

session <- Session$new("MySQL")

expect_is(session, "Session")

session$finish()
