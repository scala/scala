object Test {
	 def foo(a: String*) = a
	 val fooEta = foo _
	 (foo: Seq[String] => Seq[String])
}
