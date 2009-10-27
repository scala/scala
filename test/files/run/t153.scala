object Test extends Application {
	def powers(x: Int) = if ((x&(x-1))==0) Some(x) else None
	val res = (Stream.range(1, 1000000) flatMap powers).reverse
	println(res take 42 force)
}