trait A

class B(private val n: Int) extends AnyVal {
  def ==[T](o: T)(implicit a: A): Boolean = ???
}

case class C(b: B)

// cf test/files/pos/t10536.scala
