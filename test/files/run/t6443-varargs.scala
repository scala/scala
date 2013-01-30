object Test {

	def main(args: Array[String]) {
		def foo = {println("foo"); 0}
		lazyDep(X)(foo)
	}

  trait T {
  	type U
  }
  object X extends T { type U = Int }

	def lazyDep(t: T)(us: t.U*) {
		List(us: _*)
	}
}
