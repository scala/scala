/* Test checked initializers. Needs to be run with -Xexperimental and -checkinit
 */

// 0 inherited fields
class A {
  val x = 1
  val y = 2
  var z = 3
}

// 3 inherited fields
class B extends A {
  val b1 = 1
  var b2 = 2
}


trait T {
  val t1 = 1
  var t2 = 2
}

// Should not throw
class D extends B with T {
  val sum = x + y + z + b1 + b2 + t1 + t2
  override def toString =
    "sum = " + sum

}

abstract class NeedsXEarly {
  val x: Int
  val y = x + 1
}

// should pass
class GoodX extends { val x = 1 } with NeedsXEarly {
}

// should throw
class BadX extends NeedsXEarly {
  val x = 1
  println(y)
}

// should pass
class UglyX extends NeedsXEarly {
  lazy val x = 1
  println(y)
}

trait XY {
  val x = 1
  val y = 2
}

// needs x and y early
trait LazyFields {
  lazy val lz1 = 1
  lazy val lz2 = 2
  val x: Int
  val y: Int
  val needsSomeEarly = {
    println("x = " + x)
    println("y = " + y)
    println("lz1 = " + lz1)
    println("lz2 = " + lz2)
    x + y + lz1 + lz2
  }
}

// will fail at init
class BadMixin extends LazyFields with XY {
  println("[OK]: " + needsSomeEarly)
}

// should print 24
class GoodMixin extends {
        override val x = 10
        override val y = 11
      } with LazyFields with XY {
  println("[OK]: " + needsSomeEarly)
}

class TestInterference extends {
  override val x = 10
  override val y = 11
} with A with T with LazyFields {
  println("[OK]: " + needsSomeEarly)
}


object Test extends App {

  def shouldThrow(t: => Unit) = try {
    t
    println("[FAIL]: No UFE thrown")
  } catch {
    case UninitializedFieldError(msg) =>
      println("[OK] Caught UFE: " + msg)
  }


  val d = new D()
  println(d)

  shouldThrow(new BadX)
  (new GoodX)
  (new UglyX)

  shouldThrow(new BadMixin)
  (new GoodMixin)

  (new TestInterference)
}
