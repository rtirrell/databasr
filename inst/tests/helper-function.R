prepareStatement <- function(query) {
	str_replace_all(query, fixed("%database"), session$database)
}