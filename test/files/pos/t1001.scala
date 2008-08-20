class Foo;

object Overload{
  val foo = classOf[Foo].getConstructors()(0)
  foo.getDeclaringClass
}
