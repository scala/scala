object Test {
	 def foo(a: String*) = a
	 val fooEta = foo _
	 (foo: scala.collection.Seq[String] => scala.collection.Seq[String])
}
