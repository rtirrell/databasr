# Keep key and original key (if changed)?
PendingUpdate <- setRefClass('PendingUpdate',
	contains = c(
		'DatabasrObject'
	),
	fields = c(
		'key',
		'fields',
		'value'
	)
)