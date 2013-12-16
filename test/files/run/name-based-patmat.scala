final class MiniSome[T](val get: T) extends AnyVal { def isEmpty = false }

package p0 {
  class Single(val x: Any) extends AnyRef with Product1[String] {
    private def s = "" + x
    override def canEqual(x: Any) = this eq x.asInstanceOf[AnyRef]
    def isEmpty = false
    def get = this
    def _1 = s + " only"

    override def toString = s"Single(${_1})"
  }

  object Single {
    def unapply(x: Any): Single = new Single(x)
  }

  class SingleNoProduct(val x: Any) extends AnyRef {
    private def s = "" + x
    def isEmpty = false
    def get = s + " only, no product"

    override def toString = s"SingleNoProduct($get)"
  }

  object SingleNoProduct {
    def unapply(x: Any): SingleNoProduct = new SingleNoProduct(x)
  }
}

package p1 {
  class Triple(val x: Any) extends AnyRef with Product3[String, String, String] {
    private def s = "" + x
    override def canEqual(x: Any) = this eq x.asInstanceOf[AnyRef]
    def isEmpty = false
    def get = this
    def _1 = s
    def _2 = "2 " + s + "s! A ha ha!"
    def _3 = "3 " + s + "s! A ha ha!"

    override def toString = s"Triple(${_1}, ${_2}, ${_3})"
  }

  object Triple {
    def unapply(x: Any): Triple = new Triple(x)
  }
}

package p2 {
  class Triple(val x: Any) {
    private def s = "" + x
    def isEmpty = false
    def get = this
    def _1 = s
    def _2 = "2 " + s + "s! A ha ha!"
    def _3 = "3 " + s + "s! A ha ha!"
    override def toString = s"Triple(${_1}, ${_2}, ${_3})"
  }

  object Triple {
    def unapply(x: Any): Triple = new Triple(x)
  }
}

package p3 {
  case class Foo(x: Int, y: Int, zs: Int*)

  object Bar {
    def f(x: Foo) = x match {
      case Foo(5, 10, 15, 20, _*) => 1
      case Foo(5, 10, 15, _*)     => 2
      case Foo(5, 10, _*)         => 3
      case Foo(5, 10)             => 4 // should warn unreachable
      case _                      => 5
    }
  }
}

object Test {
  def main(args: Array[String]): Unit = {
    "catdog" match {
      case p0.Single(x) => println(s"`${x._1}` has ${x._1.length} chars")
      case x            => println("fail: " + x)
    }
    "catdog" match {
      case p0.SingleNoProduct(x) => println(s"`$x` has ${x.length} chars")
      case x                     => println("fail: " + x)
    }
    "catdog" match {
      case p1.Triple(x, y, z) => List(x, y, z) foreach println
      case x                  => println("fail: " + x)
    }
    // TODO
    "catdog" match {
      case p2.Triple(x, y, z) => List(x, y, z) foreach println
      case x                  => println("fail: " + x)
    }

    println(p3.Bar.f(p3.Foo(5, 10, 15, 20, 25)))
    println(p3.Bar.f(p3.Foo(5, 10, 15, 20)))
    println(p3.Bar.f(p3.Foo(5, 10, 15)))
    println(p3.Bar.f(p3.Foo(5, 10)))
    // println(p3.Bar.f(p3.Foo(5)))
  }
}
