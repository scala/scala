package object foo {
	def bar: Unit = {}
}

package foo {

	package bar{
		class Baz
	}
}
