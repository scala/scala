object Test extends App {

  case class Foo(s: String, n: Int)

  def foo[A, B, C](unapply1: A => Option[(B, C)], v: A) = {
    unapply1(v) match {
      case Some((fst, snd)) => println("first: " + fst, " second: " + snd)
      case _ => println(":(")
    }
  }

  foo(Foo.unapply, Foo("this might be fun", 10))
}
