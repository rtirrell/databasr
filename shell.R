library(RMySQL)
library(databasr)
session <- Session$new("MySQL")

test <- introspectTable(session, "test")

q <- session$query(test)
statement <- q$SQL()