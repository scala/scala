// SI-4560 (and SI-4601): Reflection caches are expected in the wrong classfiles
// with various differing constellations of self-types. This leads to runtime exceptions
// when the reflection caches are accessed. This tests both reflection cache accesses
// for structural type method invocations (`y.f()`) (SI-4560) and accesses to symbols which are
// handled similarly (SI-4601)

// TEST 1
// self-type is other trait


import scala.language.{ reflectiveCalls }

trait Aa
trait Ab

trait B {
  self: Aa with Ab =>

  def y = new { def f() = println("Success 1") }
  def fail() = {
    println('Test)
    y.f()
  }
}

object Test1 extends Aa with Ab with B

// TEST 2
// self-type is class

class A2

trait B2 {
  self: A2 =>

  def y = new { def f() = println("Success 2") }
  def fail() = {
    println('Test)
    y.f()
  }
}

object Test2 extends A2 with B2

// TEST 3
// self-type is singleton type

trait B3 {
  this: Test3.type =>

  def y = new { def f() = println("Success 3") }
  def fail() = {
    println('Test)
    y.f()
  }
}

object Test3 extends B3 {
  def test { fail() }
}

object Test {
  def main(args: Array[String]) {
    Test1.fail()
    Test2.fail()
    Test3.fail()
  }
}

