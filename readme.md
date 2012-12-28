#cl-parallel#

cl-parallel is an api for Common Lisp that's designed to make parallelism easy.
It uses bordeaux threads, and should work on most systems on which threads are
enabled. That being said, I've only tested it with sbcl on Fedora Linux.

##Installation (requires quicklisp)##


Clone this repository, and then run:
```lisp
(load "cl-parallel.asd")
```
If the file loaded successfully, now run:
```lisp
(ql:quickload 'cl-parallel)
```

Once that's done, you can now access all of the functions in cl-parallel.

##Synopsis##

* future - given an arbitrary number of forms, will spawn a thread to
calculate the value of that expression.

* future-p - given a form, tests whether or not it's a future.

* realize - if the argument is a future, blocks until it's thread is joined,
otherwise, just returns the argument.

* \#! (read macro) - a read macro for realize.

* par-calls - given an arbitrary number of forms, will evaluate them all  in
parallel.

* par-map - given a function, list, and optional number of threads, will apply
that function to every element in the list over the specified number of threads.

* par-map-chunked - same as par-map, but there is another parameter, the size of
the chunks that the list will be broken into for threading; often more efficient
than par-map.
