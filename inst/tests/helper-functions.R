prepare_sql <- function(value) {
	str_replace_all(value, fixed('%database'), session$database)
}

prepareSQL <- prepare_sql
prepareStatement <- prepare_sql


