load_all('.')
tryCatch(auto_test_package('.'), finally = source('shell.R'))