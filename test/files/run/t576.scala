import scala.language.reflectiveCalls

class A {
  override def equals(other: Any) = other match {
    case _: this.type => true
    case _            => false
  }
}

object Dingus {
  def IamDingus = 5
}

object Test {
  val x1 = new A
  val x2 = new A

  val x3 = new { self =>
    override def equals(other : Any) = other match {
      case that: self.type    => true
      case _                  => false
    }
  }
  val x4 = new { self =>
    def f(x: Any): Int = x match {
      case _: x1.type     => 1
      case _: x2.type     => 2
      case _: x3.type     => 3
      case _: self.type   => 4
      case x: Dingus.type => x.IamDingus
    }
  }

  def main(args: Array[String]): Unit = {

    assert(x1 == x1)
    assert(x1 != x2)
    assert(x1 != ())
    assert(x2 != x1)

    assert(x3 == x3)
    assert(x3 != x2)
    assert(x2 != x3)

    List(x1, x2, x3, x4, Dingus) map x4.f foreach println
  }
}
