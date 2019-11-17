class A {
  class C
  class Inf[X<:C](x: X) 
  def f[X<:C](x: X) = ()
}

object Test extends A {
  val c: C = new C
  f(c)
  new Inf(c)
}

/*
snips $ ~/scala-2.13.0-M2/bin/scalac -d /tmp t1479.scala 
t1479.scala:10: error: inferred type arguments [Test.C] do not conform to class Inf's type parameter bounds [X <: A.this.C]
  new Inf(c)
  ^
t1479.scala:10: error: type mismatch;
 found   : Test.C
 required: X
  new Inf(c)
          ^
two errors found
snips $ ~/scala-2.13.0-M3/bin/scalac -d /tmp t1479.scala 
t1479.scala:10: error: type mismatch;
 found   : Test.C
 required: A.this.C
  new Inf(c)
          ^
one error found
snips $ ~/scala-2.13.0-M5/bin/scalac -d /tmp t1479.scala 
*/
