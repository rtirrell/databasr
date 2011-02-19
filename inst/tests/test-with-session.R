context("Testing with(Session)")

expect_equal(with(session, "SELECT 1;")[1, ], 1)