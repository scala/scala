trait A

trait B extends Any {
  def ==[T](o: T)(implicit a: A): Boolean = ???
}

case class C(b: B)

// cf test/files/pos/t10536.scala
