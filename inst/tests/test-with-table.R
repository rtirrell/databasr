context('Testing with(*Table)')

expr <- with(db1, i1 == v1)
expect_is(expr[[1]], 'NegatableBinaryOperatorElement')

expr <- with(db1, tuple(i1 == v1) | i1 > v1)
expect_equal(length(expr), 1)
expect_is(expr[[1]], 'BinaryOperatorElement')
expect_equal(expr[[1]]$.children[[2]]$.children[[1]]$table$.name, 'databasr_test_1')

expect_is(expr[[1]]$.children[[1]], 'TupleElement')
expect_is(expr[[1]]$.children[[1]]$.children[[1]], 'NegatableBinaryOperatorElement')
expect_equal(expr[[1]]$.children[[1]]$.children[[1]]$.children[[1]]$name, 'i1')
expect_equal(expr[[1]]$.children[[1]]$.children[[1]]$.children[[2]]$name, 'v1')

expect_is(expr[[1]]$.children[[2]], 'BinaryOperatorElement')
expect_equal(expr[[1]]$.children[[2]]$.children[[1]]$name, 'i1')
expect_equal(expr[[1]]$.children[[2]]$.children[[2]]$name, 'v1')

expr <- with(db1, tuple(i1 == v1) | i1 > v1 | db2$i1 == i1)

