// this class's bytecode, compiled under -optimize is analyzed by the test
// method a's bytecode should be identical to method b's bytecode
case class Foo(x: Any)

class SameBytecode {
  def a = 
    (Foo(1): Any) match {
      case Foo(_: String) =>
    }

  // there's no null check
  def b: Unit = {
    val x1: Any = Foo(1)
    if (x1.isInstanceOf[Foo]) {
      val x3 = x1.asInstanceOf[Foo]
      if (x3.x.isInstanceOf[String]) {
        val x = ()
        return
      }
    }

    throw new MatchError(x1)
  }
}