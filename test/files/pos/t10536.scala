trait A

trait B[C <: B[C]] {
  def ==(o: C)(implicit a: A): Boolean = ???
}

trait D[C <: B[C]]

case class E[C <: B[C]](c: C) extends D[C]
