package test

class Box[T]

class Foo {
	/**
	 * This method shouldn't affect public API of Foo
	 * but due to instability of synthesized names for
	 * existentials causes change of `foo` method API.
	 */
	private def abc: Box[_] = null
	def foo: Box[_] = null
}
