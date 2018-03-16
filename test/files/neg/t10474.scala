
object Test {
  def Foo(a: Int): Char = ???

  object Bar

  def crash[A](): Boolean = Bar match {
    case Foo.Bar ⇒ true
    case _ ⇒ false
  }
}

trait hrhino {
  def Foo(i: Int) = i
  val Foo.Crash = ???
}
