import scala.annotation.compileTimeOnly
import scala.language.existentials

@compileTimeOnly("C1") class C1
object C1

class C2
@compileTimeOnly("C2") object C2

@compileTimeOnly("C3") case class C3(x: Int)

@compileTimeOnly("C4") case class C4(x: Int)
object C4

object pkg {
  @compileTimeOnly("C5")
  implicit class C5(val x: Int) {
    def ext = ???
  }
}

class C6(@compileTimeOnly("C6.x") val x: Int) {
  @compileTimeOnly("C6.foo") def foo = 2
  @compileTimeOnly("C6.Foo") type Foo = Int
  @compileTimeOnly("C6.y") var y = 3
}

@compileTimeOnly("C7") class C7
@compileTimeOnly("C8") class C8[T]

object Test extends App {
  new C1()
  C1

  new C2()
  C2

  new C3(2)
  C3(2)

  new C4(2)
  C4(2)

  import pkg._
  2.ext
  C5(2)

  val c6 = new C6(2)
  val _ = c6.x
  c6.foo
  type Foo = c6.Foo
  c6.y = c6.y

  val c701: (C7, C7) = ???
  val c702: (C7 => C7) = ???
  val c703: { val x: C7 } = ???
  val c704: AnyRef with C7 = ???
  // https://groups.google.com/forum/#!topic/scala-internals/5n07TiCnBZU
  // val c705: ({ @compileTimeOnly("C7") type C7[T] = List[T] })#C7[_] = ???
  val c706: C7 Either C7 = ???
  val c707a: List[C7] = ???
  val c707b = List[C7]()
  val c708a: T forSome { type T <: C7 } = ???
  // https://groups.google.com/forum/#!topic/scala-internals/5n07TiCnBZU
  // val c708b: T forSome { @compileTimeOnly("C7") type T } = ???
  val c709: (C8[Int], C8[C7]) = ???
  val c710: (C8[_] => C8[_]) = ???
}

@compileTimeOnly("placebo")
class placebo extends scala.annotation.StaticAnnotation

@placebo
class Test {
  @placebo def x = (2: @placebo)
}