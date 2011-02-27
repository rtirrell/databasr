.CLAUSE.ORDER <- c(
	'select', 'from', 'joins', 'where', 'group', 'having', 'order', 'limit', 'offset'
)

.OPERATOR.NAMES <- list(
	'-' = 'minus',
	'+' = 'plus',
	'/' = 'divide',
	'*' = 'multiply',
	'IN' = 'in',
	'NOT IN' = 'not_in',
	'!=' = 'not_equal',
	'==' = 'equal',
	'IS' = 'is',
	'IS NOT' = 'is_not'
)

.NEGATABLE.OPERATOR.MAP <- list(
	'IN' = 'NOT IN',
	'IS' = 'IS NOT',
	'=' = '!=',
	'NOT IN' = 'IN',
	'IS NOT' = 'IS',
	'!=' = '='
)