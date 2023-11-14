package foo

object SealedNameUsedInDefaultScope {
  def bar(a: Sealed) = a.toString

  def foo(a: Any) = a match {
    case a: Sealed =>
      1
    case _ =>
      2
  }

  val Seq(a: Sealed) = ???

  class MyHolder[A]

  val x = new MyHolder[Sealed]
}