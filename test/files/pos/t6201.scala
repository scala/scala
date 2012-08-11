class Test {
  class Foo1 {
    def must(x: scala.xml.Elem) = ()
  }

  class Foo2 {
    def must(x: Int) = ()
  }
  implicit def toFoo1(s: scala.xml.Elem) = new Foo1()
  implicit def toFoo2(s: scala.xml.Elem) = new Foo2()

  def is: Unit = { (<a>{"a"}</a>).must(<a>{"b"}</a>) }
}