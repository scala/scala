object Test {

	def main(args: Array[String]): Unit = {
		def foo = {println("foo"); 0}
		lazyDep(X)(foo)
	}

  trait T {
  	type U
  }
  object X extends T { type U = Int }

	def lazyDep(t: T)(u: => t.U): Unit = {
		println("1")
		u
		u
	}
}
