object Test {
	case class Fn[A, B](f: A => B)

	def f(x: Any) = x match { case Fn(f) => f(5) }

	Fn((x: String) => x)
}
