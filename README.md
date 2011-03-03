# databasr
databasr is a database abstraction and pseudo-ORM package for R.
Features currently supported to some degree on **MySQL** include:

* An expressive syntax using standard R functions wherever possible.
* Proxying of results and management of database sessions.
* Table introspection and, optionally, 'live' updates to tables.


## Status
This is an early release, and the package only supports MySQL at the moment. 
The database-specific portions are reasonably decoupled from the rest, 
so it shouldn't be too much work to add support for other DBMSs.


## Details
New releases of R (post 2.12) include a new class sytem, 'R5', that more 
closely approximates the semantics of an OO language. databasr builds on these as much 
as possible, and as such, we are aiming at what is very probably a moving target.


### Design overview
* All objects representing a value that may be present in a query inherit from `SQLObject`, which
  provides basic n-ary tree functionality (having `.parent` and `.children` fields).
 
* Statements correspond to `Statement`, clauses to `Clause`, and elements of a clause to
  `Element`.
  
* `Session` provides session-handling, connect and disconnecting, and so on.
 
* `Result` represents the results of a query (whether fetched or not) and provides limited
  access-based fetching.
  
* `IntrospectedTable` represents a table, `IntrospectedField` a field. 
 Each has subclasses appropriate for inclusion in a query.
  

### A day in the life of a query
* Generation: `statement <- session$select(...)$join(...)$where(...)`, and so on.

* Execution: 
  either by `statement$execute()`, `statement$all()`, `statement$first()`, or `statement$one()`.
  The latter three of these just call `execute()` and fetch immediately (no lazy behavior).
  `one()` also ensures that the result set has exactly one row.
  The return value of `execute()` is either a `data.frame` or `Result` object (see below).
  
  * In `SQL()`, `prepare()` is first called down the tree from the parent statement.
    `prepare()` infers tables when they are not specified (and they should rarely need be).
    It also checks for ambiguity of field names and sets options which the compiler checks.
  
  * Then (and this is not implemented currently as there appears to be no need as yet), 
    `dbPrepare()` is called down the tree with database-specific compilation 
    information attached to an object that rides down.
    
  * A instance of the `Compiler` class is created, which is passed the `Statement` object and an
    instance of the `Formatter` class.
    The compiler the tree and generates database-specific SQL from the constructs represented by
    the statement and its children.
    
* Return value: ...


## TODO
* Adopt a unified approach to aliasing. 
  This is difficult given lack of multiple inheritance or mixins.
  
* Some kind of flush-by-interval or flush-by-count system. 
  This is not too hard to actually implement, but to do well requires some more thought.
 
* Reconnecting of expired sessions? 
  I remain wary of the warnings routinely thrown by Session, but I'm not sure they are avoidable
  
* Balance of sanity-checking at generation or compilation time versus at execution time (i.e.
  "You have an error in your SQL syntax").
  
* Support for raw SQL.

* Continued work on lazy results.


## Need tests
* `PrefixOperatorElement`.

* `GROUP BY`.

* `HAVING` -- the tests for `WHERE` mostly cover it.

* Compilation of aliased operators.
