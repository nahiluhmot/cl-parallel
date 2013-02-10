#cl-parallel#

cl-parallel is an API for Common Lisp that's designed to make parallelism easy.
It uses [bordeaux-threads][bt], and should work on most systems on which threads are
enabled. That being said, I've only tested it with sbcl on Fedora Linux and
OSX.

##Installation (requires quicklisp)##

Now that this project is in quicklisp, simply run:
```lisp
(ql:quickload 'cl-parallel)
```

##Making cl-parallel available##

Once that's done, you can now access all of the functions in
cl-parallel, which are in the `PARALLEL` package.

To use cl-parallel in your packages, try

```lisp
(defpackage :your-package
  ...
  (:use :parallel)
  ...
)
```

If you don't want to import all symbols exported by cl-parallel, try

```lisp
(defpackage :your-package
  ...
  (:import-from :parallel #:par-map-reduce)
  ...
)
```

which imports only `par-map-reduce` into your pakage.

##Synopsis##

###Low Level 'future' API###

* future - given an arbitrary number of forms, spawn a thread to calculate
the value of the implicit progn made up of the forms.

* future-p - given a form, test whether it's a future.

* future-finished-p - given a form, return `T` iff it's a future that has
finished executing.

* realize - if the argument is a future, block until its thread is
joined; otherwise, return the argument.

* realize-if-finished - given a future, realize it iff it has finished
executing.  If the argument is not a future, it is returned.

* \#! (read macro) - synonym for realize.

###Actual parallelism API###

All of the below functions takes the optional keywords `:max-threads` (default 4)
and `:sleep-time` (default 0):

* par-calls - given an arbitrary number of forms, evaluate them all in
parallel.

* par-map - given a function, and a list, apply that function to every
in the list.

* par-some - given a predicate and list, return `T` if any of the
elements in the list satisfy the predicate; `NIL` otherwise.

* par-every - given a predicate and list, return `T` if all of the
elements in the list satisfy the predicate; `NIL` otherwise.

* par-find-if - given a predicate and list, return an element in
the list that satisfies the predicate; `NIL` otherwise. Unlike
in the standard `FIND-IF`, the value returned by this function may not
be the leftmost element of the list satisfying the predicate (if there is
such an element).

* par-find - given an item and list, return the item if it is in
the list; `NIL` otherwise.

* par-map-chunked - same as par-map, but there is another keyword
parameter, `:chunk-size`. This parameter denotes the size of the
chunks that will be processed in parallel. This function is often more
efficient than par-map.

* par-map-reduce - given a mapping function, a reducing function, an initial
value, and a list, map the first function across the list in parallel and
collects the result using the reducing function. Note that this function will
call the reducing function in no specific order.

[bt]: http://common-lisp.net/project/bordeaux-threads/ "Bordeaux Threads"
