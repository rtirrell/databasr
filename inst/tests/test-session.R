context("Testing Session")

expect_is(session, "Session")

connection <- session$request("test")
expect_true("test" %in% session$users)
session$release(connection)
expect_false("test" %in% session$users)