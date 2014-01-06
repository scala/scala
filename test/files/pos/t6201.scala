// probably needs xml's weirdness to reproduce
// (specifically, _root_.scala.xml.Null being in the root package)
class Elem

class Test {
  def elem: Elem = ???

  class Foo1 {
    def must(x: Elem) = ()
  }

  class Foo2 {
    def must(x: Int) = ()
  }
  implicit def toFoo1(s: Elem) = new Foo1()
  implicit def toFoo2(s: Elem) = new Foo2()

  def is: Unit = { (elem) }
}