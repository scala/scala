trait Foo[X] { def foo : Map[String,Foo[X]] }
object T3560 { def f[T]() : Foo[T] = new Foo[T] { var foo = Map[String,Foo[T]]() } }
