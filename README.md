# databasr
databasr is a database abstraction and ORM for R.
Features currently supported to some degree on **MySQL** include:

* An expressive syntax using standard R functions wherever possible.
* Proxying of results and pseudointelligent handling of database sessions.
* Table introspection and, optionally, 'live' updates to tables.

## Status
This is an early release, and the package only supports MySQL at the moment. 
The database-specific portions are reasonably decoupled from the rest, 
so it shouldn't be too much work to add support for other DBMSs.


## Details
New releases of R (post 2.12) include a new class sytem, 'R5', that more 
closely approximates the semantics of an OO language. databasr builds on these as much 
as possible, and as such, we are aiming at what is very probably a moving target.

### Design Overview
* All objects representing a value that may be present in a query inherit from `SQLObject`, which
  provides basic n-ary tree functionality (having `.parent` and `.children` attributes).
* Statements correspond to `Statement`, clauses to `Clause`, and elements of a clause to
  `Element`.
* `Session` provides session-handling, connect and disconnecting, and so on.
* `Result` represents the results of a query (whether fetched or not) and provides limited
  access-based fetching.
* `IntrospectedTable` represents a table, `IntrospectedField` a field. Each of these have
  subclasses appropriate for inclusion in a query.
  

### A day in the life of a query
* Generation: `statement <- session$query(...)$join(...)$where(...)`, and so on.
* Execution: either by `statement$execute()`, `statement$all()` or `statement$one()`.
  The latter two of these just call `execute()` and fetch immediately.
  Execution creates a new `Result` object, which calls `SQL()` on the statement it is passed.
  * In `SQL()`, `prepare()` is first called down the tree from the parent statement.
  * Then (and this is not implemented currently as there appears to be no need as yet), 
    `dbPrepare()` is called down the tree with database-specific compilation 
    information attached to an object that rides down.
  * A instance of the `Compiler` class is created, which is passed the `Statement` object and an
    instance of the `Formatter` class.
    The compiler the tree and generates database-specific SQL from the constructs represented by
    the statement and its children.
* If called with `mutable = TRUE`, the statement is checked to see whether the result could
  possibly be mutated (updated or inserted).
  As rows of the result are accessed, we fetch segments of the result set. 
  Note then that we are assuming sequential access, for now.
* If the statement was execute with `mutable = TRUE` and meets all of requirements 
  (single table, full key, etc.), modification of the result adds an `UpdateStatement` 
  to a list of pending mutations associated with the result.
* These mutations can be flushed manually by calling `flush()`.

## TODO
* Unified approach to aliasing - difficult given lack of multiple inheritance or mixins.
* Tests for GROUP BY, HAVING, getWith, sendWith, compilation of aliased operators, on-demand
  fetching.
* Distinct for statements, and dispatch on dots.
* Using with in query to generate SELECTs.
* Some kind of flush-by-interval or flush-by-count system.
* Reconnecting of expired sessions? I remain wary of the warnings routinely thrown by Session, but
  I'm not sure they're avoidable.
  
## Ideas
* The SQLDF approach? Loading tables from data frames -- and then query over data frames
  as well as introspected tables.