About
=====

This program is an interpreter for the language LANG which I wrote for
the class [CS510](https://web.stevens.edu/compsci/graduate/masters/courses/viewer.php?course=CS510&type=syl) (Principles of Programming Languages) at Stevens Institute
of Technology in the Spring of 2013.
The class was taught by [Adriana Compagnoni](http://www.cs.stevens.edu/~abc/Adriana_Compagnoni/).

LANG is a simple programming language supporting addition, subtraction, assignment to zero, assignment of variable value, NOP, if-GOTO, GOTO.

The interpreter has the following main features:

* allows to write LANG programs in Scheme
* pretty-prints LANG programs
* parses S-list representation of LANG programs
* supports shorthand notations of LANG
* expands shorthand notations of LANG
* evaluates LANG programs

The main file is called `FoergFabian.FP.rkt`.
The test file is located under the sub-folder `testcases`.

Expansion Functions
===================

expand-GOTOS
------------

The function expand-GOTOS expands a labeled GOTO

	(A) GOTO B

the following way:

	(A) Z <- Z + 1
	    IF Z =/= 0 GOTO B

where `Z` is a fresh variable.

expandV<-0
----------

The function expand<-0 expands an unlabeled zero-assignment

	V <- 0

as follows:

	    GOTO Z2
	(Z1) V <- V - 1
	(Z2) IF V =/= 0 GOTO Z1

where Z1 and Z2 are fresh labels.
We do the expansion as described above in order to avoid number
underflows which occur when the following expansion is used and
V is equal to zero or negative:

	(Z1) V <- V - 1
	    IF V =/= 0 GOTO Z1

The function expandV<-0 expands a labeled zero-assignment

	(A) V <- 0

the following way:

	(A) GOTO Z2
	(Z1) V <- V - 1
	(Z2) IF V =/= 0 GOTO Z1

expand-assign
-------------

One possible way to expand

	V <- V'

is the following routine (expand-assign V V'):

	    V <- 0
	    Z <- 0
	    IF V' =/= 0 GOTO A
	    GOTO B
	(A) V' <- V' - 1
	    V <- V + 1
	    Z <- Z + 1
	    IF V' =/= 0 GOTO A
	(B) V <- V

where `A` and `B` are fresh labels and `Z` is a fresh variable.

Now `V` and `Z` are equal to the original `V'`, but `V'` became
zero.
This may change the output of the program, if `V'` occurs in
the rest of the program.

To solve this problem, we can do the following:

	(expand-assign V V')
	(expand-assign V' Z)

Note that expand-assign produces zero-assignments and
GOTOs. These shorthands may be expanded afterwards.
Moreover, `Z` must not be used again somewhere in the rest
of the program.

clean-up
--------

The function `clean-up p` returns a program that produces
the same output as p and does not contain shorthands.

Testing
=======

Just run the file inside the sub-folder `testcases` inside
DrRacket.
The program displays either that all tests completed successfully
or prints the number of the first test case that failed.
