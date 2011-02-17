library(testthat)
library(databasr)

session <- Session$new("MySQL")
for (i in 1:4)
	assign(sprintf("db%d", i), introspectTable(session, sprintf("databasr_test_%d", i)))