trait Outer[O] {
  case class C[T <: O](x: T, y: Outer.this.type)
}

object Foo extends Outer[String]

class Tst {
  val c = new Foo.C("a", Foo)
  c match {
    case Foo.C(a, o) => a
  }
}
