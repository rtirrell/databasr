#' Class wrapping a connection object with some minimal information on 
#' its parameters and position and its position in the session's connection
#' list.
Connection <- setRefClass('Connections',
	contains = c(
		'DatabasrObject'
	),
	fields = c(
		'connection',
		'index',
		'parameters'
	),
	
	methods = list(
		initialize = function(connection, index, parameters) {
			initFields(
				connection = connection, 
				index = index, 
				parameters = parameters
			)
		}
	)
)