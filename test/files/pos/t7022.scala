class Catch[+T] {
	def either[U >: T](body: => U): Either[Throwable, U] = ???
}

object Test {
	implicit class RichCatch[T](val c: Catch[T]) extends AnyVal {
	  def validation[U >: T](u: => U): Either[Throwable, U] = c.either(u)
	}
}
