context("Testing Session")

expect_is(session, "Session")

expect_equal(with(session, "SELECT 1;")[1, ], 1)

connection <- session$request("test")
expect_true("test" %in% session$users)
session$release(connection)
expect_false("test" %in% session$users)