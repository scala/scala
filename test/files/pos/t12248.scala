trait A

trait B {
  def ==[T](o: T)(implicit a: A): Boolean = ???
}

case class C(b: B)

// cf test/files/pos/t10536.scala
