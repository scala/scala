abstract class Extensible[A, This <: Extensible[A, This]](x: A, xs: This) { self: This =>
	def mkObj(x: A, xs: This): This;
}
class Fixed[A](x: A, xs: Fixed[A]) extends Extensible[A, Fixed[A]](x, xs) {
	def mkObj(x: A, xs: Fixed[A]) = new Fixed(x, xs);
}
