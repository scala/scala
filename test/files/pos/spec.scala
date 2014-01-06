



class Bar[@specialized(Int, AnyRef) A](a: A) {
  val memb = a
}


class WithInner[@specialized(Int, AnyRef) A](a: A) {
  class Inner {
    def meth = a
  }
}


class Baz[@specialized(Int, AnyRef) A, @specialized(Int, AnyRef) B] {
  def ab(a: A, b: B) = (a, b)
}


trait Base[@specialized(Int, AnyRef) A]
class Concrete[@specialized(Int, AnyRef) A] extends Base[A]


class WithAnon[@specialized(Int, AnyRef) A](a: A) {
  new AnyRef {
    def foo = a
  }
}


class Norm {
  def id[@specialized(Int, AnyRef) A](a: A) = a
}


class Qux[@specialized(AnyRef) A] {
  def memb[@specialized(AnyRef) B](a: A, b: B) = (a, b)
}


class Foo[@specialized(Int, AnyRef) A](val a: Array[A]) {
  a(0)

  def id(elem: A) = a(0) = elem
}


// instantiation and selection
object Test {
  def main(arg: Array[String]) {
    val f = new Foo(new Array[String](5))
    f.id("")

    val z = new Baz[Int, Double]
    z.ab(1, 1.0)

    testspec(new Array[String](5))
    testspec(new Array[Int](5))
  }

  def testspec[@specialized(Int, AnyRef) T](arr: Array[T]) = arr(0)
}
