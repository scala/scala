final case class V[A](x: A) extends AnyVal {
  def flatMap[B](f: A => V[B]) = if (true) this else f(x)
}
