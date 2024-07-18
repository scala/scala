
//> using options -Yrangepos
//
class Foo {
  class Bar
  object Bar {
    implicit def fromString(a: String) = new Bar
  }
  def andThen(b : Bar) = b
  def andThen1(i : Int)(b : Bar) = b
  def andThen2(b : Bar)(implicit dummy: DummyImplicit) = b
  def andThen3[T](b: Bar) = b
}

object Test {
  (new Foo) andThen ("Bar")
  (new Foo).andThen1(23)("Bar")
  (new Foo) andThen2 ("Bar")
  (new Foo) andThen3[Int]("Bar")
}
