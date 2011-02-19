context("Testing finishing of Session")
expect_false(session$getOption("finished"))
session$finish()
expect_true(session$getOption("finished"))
