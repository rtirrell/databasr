Type <- setRefClass('Type'
)

NumericType <- setRefClass('NumericType',
	fields = c(
		'bytes'
	),
	contains = c(
		'Type'
	)
)

RealType <- setRefClass('RealType',
	fields = c(
		'decimal.bytes'
	),
	contains = c(
		'NumericType'
	)
)

IntegerType <- setRefClass('IntegerType',
	fields = c(
		'signed'
	),
	contains = c(
		'NumericType'
	)
)

StringType <- setRefClass('StringType',
	contains = c(
		'Type'
	)
)

CharacterType <- setRefClass('CharacterType',
	fields = c(
		'bytes'
	),
	contains = c(
		'StringType'
	)
)

TextType <- setRefClass('TextType',
	contains = c(
		'StringType'
	)
)

createType <- function(type.string) {
	type.string <- tolower(type.string)
	if (str_detect(type.string, fixed("int"))) {
		type.info <- str_match_all(type.string, "\\w*int\\((\\d+)\\) ?(\\w*)")
		type <- IntegerType$new(
			bytes = as.integer(type.info[[1]][2]), signed = type.info[[1]][3] != "unsigned"
		)
	} else if (str_detect(type.string, fixed("char"))) {
		type.info <- str_match_all(type.string, "\\w*char\\((\\d+)\\)")
		type <- CharacterType$new(bytes = as.integer(type.info[[1]][2]))
	} else if (str_detect(type.string, fixed("decimal"))) {
		type.info <- str_match_all(type.string, "decimal\\((\\d+), (\\d+)\\)")
		type <- RealType$new(
			bytes = as.integer(type.info[[1]][2]), decimal.bytes = as.integer(type.info[[1]][3])
		)
	} else if (str_detect(type.string, fixed('text'))) {
		type <- TextType$new()
	} else {
		type <- Type$new()
	}
	type
}