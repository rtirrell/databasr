library(RMySQL)
library(databasr)
session <- Session$new("MySQL")

test <- introspectTable(session, "test")
