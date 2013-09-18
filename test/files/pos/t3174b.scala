trait Foo[X] { def foo : Map[String,Foo[X]] }

object Test {
  def f[T]() : Foo[T] = {
    class Anon extends Foo[T] {
      var foo: Map[String, Foo[T]] = Map[String,Foo[T]]()
      //def foo = Map[String,Foo[T]]()
      //def foo_=(x: Map[String,Foo[T]]) {}
    }
    new Anon
  }
}
