trait A {
	private val foo = 12
	// we need to access foo to trigger AbstractMethodError
	def bar: Int = foo
}
