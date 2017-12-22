package t10474

object Test {
  def Foo(a: Int): Char = ???

  object Bar

  def crash[A](): Boolean = Bar match {
    case Foo.Bar ⇒ true
    case _ ⇒ false
  }

  type Crash = Foo.type

  def bingo(l: Int, r: Int) = l + r

  val `bingo`.Fuzz = 1
}