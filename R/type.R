Type <- setRefClass('Type'
)

NumericType <- setRefClass('NumericType',
	fields = c(
		'signed'
	),
	contains = c(
		'Type'
	)
)

RealType <- setRefClass('RealType',
	contains = c(
		'NumericType'
	)
)

DecimalType <- setRefClass('DecimalType',
	contains = c(
		'RealType'
	),
	fields = c(
		'bytes',
		'decimal.bytes'
	)
)

FloatType <- setRefClass('FloatType',
	contains = c(
		'RealType'
	)
)

IntegerType <- setRefClass('IntegerType',
	fields = c(
		'bytes'
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
	checkSigned <- function(type) {
		if (str_detect(type.string, fixed("unsigned"))) type$signed <- FALSE
		else type$signed <- TRUE
		type
	}
		
	type.string <- tolower(type.string)
	type <- Type$new()
	
	if (str_detect(type.string, fixed("int"))) {
		type.info <- str_match_all(type.string, "\\w*int\\((\\d+)\\).*")
		type <- checkSigned(IntegerType$new(bytes = as.integer(type.info[[1]][2])))
	} 
	
	if (str_detect(type.string, fixed("char"))) {
		type.info <- str_match_all(type.string, "\\w*char\\((\\d+)\\)")
		type <- CharacterType$new(bytes = as.integer(type.info[[1]][2]))
	} 
	
	if (str_detect(type.string, fixed("decimal"))) {
		type.info <- str_match_all(type.string, "decimal\\((\\d+),(\\d+)\\).*")
		type <- checkSigned(DecimalType$new(
			bytes = as.integer(type.info[[1]][2]), decimal.bytes = as.integer(type.info[[1]][3])
		))
	}  
	
	if (str_detect(type.string, "float|double")) {
		type <- checkSigned(FloatType$new())
	}
	
	if (str_detect(type.string, fixed('text')))
		type <- TextType$new()
	
	type
}