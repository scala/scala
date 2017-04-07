import annotation.unchecked

class Test {
  trait Two[A, B]
  type One[A] = Two[A,A]
  class View extends One[Any]

  def checkAny(x: Some[One[Any]]) = x match { // okay
    case Some(_: View) => true
    case _ => false
  }
  def checkAbstract[A](x: Some[One[A]]) = x match { // okay
    case Some(_: View) => true
    case _ => false
  }

  def checkExistential(x: Some[One[_]]) = x match {
    case Some(_: View) => true // compiler crash
    case _ => false
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    val t1 = new Test
    val t2 = new Test
    assert(t1.checkAny(Some(new t1.View)))
    assert(t1.checkAbstract(Some(new t1.View)))
    assert(t1.checkExistential(Some(new t1.View)))
  }
}
