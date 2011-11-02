object Foo { private def bar(): Int = 55 }
class Foo(x: Int) { def this() = this(Foo.bar()) }

/*
 * scalac28 a.scala 
a.scala:2: error: method bar cannot be accessed in object Foo
class Foo(x: Int) { def this() = this(Foo.bar()) }
                                          ^
one error found
*/
