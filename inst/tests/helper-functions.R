prepare_sql <- function(value) {
	str_replace_all(value, fixed('%database'), session$database)
}
