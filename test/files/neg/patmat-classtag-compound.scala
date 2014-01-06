object Test extends App{
  trait Bar
  trait Foo
  // Failed to give an unchecked warning pre: https://github.com/scala/scala/pull/2848
  //
  // Features interacting:
  //   - implicit class tags to enable type patterns on abstract types
  //   - type tests on compound types.
  //
  // We could try make these work together, but an unchecked warning is okay for now.
  def x[A: reflect.ClassTag](a: Any): Boolean = a match{
    case b: A with Bar => true
    case _ => false
  }
  println(x[Foo](new Bar{}))
  println(x[String](""))
}
