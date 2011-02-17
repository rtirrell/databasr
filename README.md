# databasr
databasr is a database abstraction and ORM for R.
Features currently supported to some degree on **MySQL** include:

* An expressive syntax using standard R functions wherever possible.
* Proxying of results and pseudointelligent handling of database sessions.
* Table introspection and, optionally, 'live' updates to tables.

## Status
This is an early release, and as noted only really supports MySQL presently. 
The backend is partially decoupled, and with some (reasonable) amount of effort could be fully
decoupled to support other RDMBS.

## Details
New releases of R (post 2.12) include a new class sytem, dubbed 'R5', that much more closely approximates the semantics of your favorite OO languages.
databasr builds on these as much as possible, and as such, we are aiming at what is very probably a moving target.

### Overview
* Statements are represened (aptly enough) by objects inheriting from `Statement`. `Statement`
  itself inherits from `SQLObject`, which represents any value that may be present in a query.
* `SQLObject`, then, is little more than a generic n-ary tree structure.
* All `SQLObject` objects have children (`.children`), which in turn contain other children, and
  so on. Leaves may also contain R literals (`integer`, `numeric`, `TRUE`/`FALSE`/`NA`, 
  `character`, etc.). Note here that we use `NA`, which seems to corresponding better to a RDBMS'
  `NULL`.
* Compilation of a statement occurs when calling the `SQL` method a statement. All `SQLObject`s 
  have a `prepare` method, which is called down the tree to inspect other objects in the tree and
  set behavior and values appropriately (e.g., we may infer `FROM` tables from those mentioned in 
  other clauses). Some statements (e.g. `SelectStatement`s) will want to restore state after
  compilation, to support reuse of the statement with further or different clauses attached, and
  the `restore` method (which is not called down the tree) is responsible for this.
* Tables are represented by `Table` objects. `Table` overloads the `$` operator, and checks in a
  `.fields` list for any attribute, returning the `Field` object given by that name preferentially.
  No checks are yet performed that a field's name does not trample on another from the class.
* Fields are represented by `Field` objects, which also have an attribute of class `Type`
  representing the underlying storage mode.
* Results of `SELECT` statements are represented by `Result` objects, which perform some limited
  (and perhaps troublesome) pseudo-intelligent fetch and update/insert operations (in-progress to
  some degree).

### A day in the life of a query

## TODO
* Unified approach to aliasing?
* Self-JOINs.
* Tests for GROUP BY and HAVING, as well as session management.
* Use plyr in some cases.