object Test extends App {
	def powers(x: Int) = if ((x&(x-1))==0) Some(x) else None
	val res = (Stream.range(1, 500000) flatMap powers).reverse
	println(res take 42 force)
}
