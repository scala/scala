trait Foo[T]
object Foo {
	implicit def javaEnumFoo[T <: java.lang.Enum[_]]: Foo[T] = ???
	implicit def stringFoo: Foo[String] = ???
}

object Test {
	// -Ytyper-debug output shows whether or not `javaEnumFoo` is considered
	// By making `isImpossibleSubtype` a little smarter, we can exclude it
	// on the grounds that `String` can't be a subtpe of the bounds ot `Enum[_]`.
	implicitly[Foo[String]]
}
