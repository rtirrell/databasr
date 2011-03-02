context('Testing finishing of Session')
expect_false(session$get_option('finished'))
session$finish()
expect_true(session$get_option('finished'))
