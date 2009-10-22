object Test {
	def test[CC[+X] <: Iterable[X], A](xs: CC[A]): CC[A] = xs
	val xs = test(List(1,2))
	val xs2: List[Int] = test(List(1,2))
}
