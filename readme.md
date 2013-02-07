#cl-parallel#

cl-parallel is an API for Common Lisp that's designed to make parallelism easy.
It uses bordeaux-threads, and should work on most systems on which threads are
enabled. That being said, I've only tested it with sbcl on Fedora Linux and
OSX.

##Installation (requires quicklisp)##

Now that this project is in quicklisp, simply run:
```lisp
(ql:quickload 'cl-parallel)
```

Once that's done, you can now access all of the functions in cl-parallel. Note
that all of the below functions are in the parallel package.

##Synopsis##

###Low Level 'future' API###

* future - given an arbitrary number of forms, will spawn a thread to calculate
the value of that expression.

* future-p - given a form, tests whether or not it's a future.

* future-finished-p - given a form, return t iff it's a future that has
finished executing.

* realize - if the argument is a future blocks until it's thread is
joined, otherwise, just returns the argument.

* realize-if-finished - given a form, realize it iff it's a future that has
finished executing. Otherwise, return the form passed in.

* \#! (read macro) - synonym for realize.

###Actual parallelism API###

All of the below functions takes the optional keywords :max-threads (default 4)
and :sleep-time (default 0):

* par-calls - given an arbitrary number of forms, will evaluate them all in
parallel.

* par-map - given a function, and a list, will apply that function to every
in the list.

* par-some - given a predicate and list, will return 'true' if any of the
elements in the list satisfy the predicate; nil otherwise.

* par-every - given a predicate and list, will return 'true' if all of the
elements in the list satisfy the predicate; nil otherwise.

* par-find-if - given a predicate and list, will return an element in the list
that satisfies the predicate; nil otherwise. Note that this does not guarantee
the first element satisfying the predicate is returned.

* par-find - given an item and list, will return the item if it is in
the list; nil otherwise.

* par-map-chunked - same as par-map, but there is another keyword  parameter,
:chunk-size. This parameter denotes the size of the chunks that will be
processed in parallel. This function is often more efficient than par-map.

* par-map-reduce - given a mapping function, reducing function, initial
value, and list, maps the first function accross the list in parallel and
collects the result using the reducing function. Note that this function will
call the reducing function in no specific order.
