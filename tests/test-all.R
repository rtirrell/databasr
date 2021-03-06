library(testthat)
library(databasr)
library(stringr)

session <- Session$new('MySQL')

cat('Generating random fixtures.\n')
connection <- session$request()

for (table in str_c('databasr_test_', 1:4)) 
	if (dbExistsTable(connection$connection, table)) 
		dbRemoveTable(connection$connection, table)


dbSendQuery(connection$connection, 'CREATE TABLE `databasr_test_1` (
	`i1` int(10),
	`v1` varchar(255),
	PRIMARY KEY (`i1`)
);'
)
dbSendQuery(connection$connection, 'CREATE TABLE `databasr_test_2` (
	`i1` int(20),
	`v1` varchar(255),
	`v2` varchar(255),
	PRIMARY KEY (`i1`, `v1`)
);'
)
dbSendQuery(connection$connection, 'CREATE TABLE `databasr_test_3` (
	`i1` int(10),
	`d1` decimal(10, 5),
	PRIMARY KEY (`i1`)
);'
)

table.size <- 1000

generate_random_strings <- function(n, lengths = c(1, 255)) {
	letters <- c(LETTERS, tolower(LETTERS), 1:9)
	replicate(
		n, str_c(
			sample(letters, sample(lengths[1]:lengths[2], 1), replace = TRUE), 
			collapse = ''
		)
	)
}
	

table <- data.frame(
	i1 = sample(1:(table.size * 10), table.size), v1 = generate_random_strings(table.size)
)
dbWriteTable(connection$connection, 'databasr_test_1', table, row.names = FALSE, append = TRUE)

table$i1 <- table$i1 * sample(1:(table.size / 10), nrow(table), replace = TRUE)
table$v2 <- generate_random_strings(table.size)
dbWriteTable(connection$connection, 'databasr_test_2', table, row.names = FALSE, append = TRUE)

# TODO: not that we really care, but this table doesn't look right.
table <- data.frame(
	i1 = sample(1:(table.size * 10), table.size),
	d1 = sample(1:(table.size * 10), table.size) / sample(1:(table.size), table.size)
)
dbWriteTable(connection$connection, 'databasr_test_3', table, row.names = FALSE, append = TRUE)

table$v1 <- generate_random_strings(table.size, c(200, 400))
dbWriteTable(connection$connection, 'databasr_test_4', table, row.names = FALSE, overwrite = TRUE)

session$release(connection)
suppressMessages(test_package('databasr'))
