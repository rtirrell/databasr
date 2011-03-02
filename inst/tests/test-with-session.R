context('Testing with(Session)')

expect_equal(get_with(session, 'SELECT 1;')[1, ], 1)

handle <- send_with(session, 'SELECT 1;')
result <- fetch(handle, -1)
expect_equal(result[1, ], 1)