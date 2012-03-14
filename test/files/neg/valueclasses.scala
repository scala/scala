package test

trait T extends AnyVal // fail

class Foo {
  class Bar(x: Int) extends AnyVal // fail
  def foo() {
    class Baz(x: Int) extends AnyVal // fail
  }
}

class V1 extends AnyVal // fail

class V2(private[test] val x: Int) extends AnyVal // fail
class V3(protected[test] val x: Int) extends AnyVal // fail
class V4(protected val x: Int) extends AnyVal // fail
class V5(private val x: Int) extends AnyVal // fail

class V6(val x: Int, val y: String) extends AnyVal // fail
class V7(val x: Int, private[this] val y: String) extends AnyVal // fail
class V8(var x: Int) extends AnyVal // fail

class V9(val x: Int) extends AnyVal {
  val y = x    // fail
}

class V10[T](val x: T)  extends AnyVal // ok
class V11[T](val x: List[T]) extends AnyVal // ok
class V12[@specialized T, U](val x: (T, U)) extends AnyVal // fail

class V13(x: Int) extends AnyVal // fail


package time {

object TOD {
 final val SecondsPerDay = 86400

 def apply(seconds: Int) = {
   val n = seconds % SecondsPerDay
   new TOD(if (n >= 0) n else n + SecondsPerDay)
 }
}

final class TOD private (val secondsOfDay: Int) extends AnyVal { // should fail with private constructor
 def hours = secondsOfDay / 3600
 def minutes = (secondsOfDay / 60) % 60
 def seconds = secondsOfDay % 60

 override def toString = "%02d:%02d:%02d".format(hours, minutes, seconds)
}
}


