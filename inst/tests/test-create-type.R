context("Testing creation of Types")

type.info <- data.frame(
	string = c(
		'decimal(1,0)', 'decimal(1,1)', 'decimal(11,0)', 'decimal(11,1)', 'decimal(11,11)'
	),
	bytes = c(1, 1, 11, 11, 11),
	decimal.bytes = c(0, 1, 0, 1, 11)
)
for (i in seq_along(type.info)) {
	type <- createType(type.info$string[i])
	expect_is(type, 'RealType')
	expect_equal(type$bytes, type.info$bytes[i])
	expect_equal(type$decimal.bytes, type.info$decimal.bytes[i])
}


type.info <- data.frame(
	string = c(
		'int(1) unsigned', 'int(1)', 'int(11) unsigned', 'int(11)',
		'mediumint(1) unsigned', 'mediumint(1)', 'mediumint(11) unsigned', 'mediumint(11)'
	),
	bytes = rep(c(1, 1, 11, 11), times = 2),
	signed = rep(c(FALSE, TRUE), times = 4)
)

for (i in seq_along(type.info)) {
	type <- createType(type.info$string[i])
	expect_is(type, 'IntegerType')
	expect_equal(type$bytes, type.info$bytes[i])
	expect_true(type$signed == type.info$signed[i])
}

type.info <- data.frame(
	string = c(
		'varchar(1)', 'varchar(11)', 'char(1)', 'char(11)'
	),
	bytes = rep(c(1, 11), times = 2)
)

for (i in seq_along(type.info)) {
	type <- createType(type.info$string[i])
	expect_is(type, 'StringType')
	expect_equal(type$bytes, type.info$bytes[i])
}