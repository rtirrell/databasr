context("Testing finishing of Sessions")
expect_false(session$getOption("finished"))
session$finish()
expect_true(session$getOption("finished"))
