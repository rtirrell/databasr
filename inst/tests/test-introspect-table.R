context("Testing introspection of tables")

expect_is(db1, "IntrospectedTable")
expect_equal(db1$.key, 1)

expect_is(db1$i1$type, "IntegerType")
expect_equal(db1$i1$type$bytes, 10)
expect_is(db1$v1$type, "CharacterType")
expect_equal(db1$v1$type$bytes, 255)

expect_equal(db2$.key, c(1, 2))

expect_equal(db3$d1$type$bytes, 10)
expect_equal(db3$d1$type$decimal.bytes, 5)

expect_is(db4$i1$type, "IntegerType")
expect_is(db4$d1$type, "RealType")
expect_is(db4$v1$type, "TextType")