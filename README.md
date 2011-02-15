# databasr
databasr is a database abstraction and ORM for R.
Features currently supported to some degree on **MySQL** include:
* An expressive syntax using standard R functions wherever possible.
* Table introspection and, optionally, 'live' updates to tables.

## Status
This is an early release, and as noted only really supports MySQL presently. 
The backend is partially decoupled, and with some (reasonable) amount of effort could be fully
decoupled to support other RDMBS.

## Details
New releases of R (post 2.12) include a new class sytem, dubbed 'R5', that much more closely approximates the semantics of your favorite OO languages.
databasr builds on these as much as possible, and as such, we are aiming at what is very probably a moving target.
