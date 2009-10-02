scala> def foo() = { println("foo") ; 5 }
foo: ()Int

scala> class C { def m1(f: => Int) = () ; def m2_:(f: => Int) = () }
defined class C

scala> val c = new C
c: C = C@96d484

scala> c m1 foo()

scala> foo() m2_: c
foo
