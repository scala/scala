class Foo[@specialized(Int) T](_x: T) {
  val x = _x
  def bar {}

  val y = x
  println(x)
  println(y)

  def baz {}
  val z = y
  println(z)
}

class Bar[@specialized(Int) T] {
  def foo(x: T) = print(x)
}

object Global {
  var msg = "ok"
}

class TouchGlobal[@specialized(Int) T](_x: T) {
  println(Global.msg)
  val x = {
    Global.msg = "not ok"
    _x
  }
}

object Test {
  def main(args: Array[String]) {
    (new Foo("abc"))
    println("shouldn't see two initialized values and one uninitialized")
    (new Foo(42))

    (new TouchGlobal(new Object))
    Global.msg = "ok" // reset the value
    (new TouchGlobal(42))
    
    println(runtime.BoxesRunTime.integerBoxCount)
  }
}
