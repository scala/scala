// by-name argument incorrectly evaluated on :-ending operator
// Reported by:   extempore   Owned by:   odersky
// Priority:  normal  Component:  Compiler
// Keywords:    Cc:   paulp@…
// Fixed in version:  
// Description

scala> def foo() = { println("foo") ; 5 }
foo: ()Int

scala> class C { def m1(f: => Int) = () ; def m2_:(f: => Int) = () }
defined class C

scala> val c = new C
c: C = C@96d484

scala> c m1 foo()

scala> foo() m2_: c
foo

// But it is not evaluated if invoked directly:

scala> c.m2_:(foo())

// scala>

