trait Foo extends Any { override def equals(x: Any) = false }
trait Ding extends Any { override def hashCode = -1 }

class Bippy1(val x: Int) extends AnyVal with Foo { }  // warn
class Bippy2(val x: Int) extends AnyVal with Ding { } // warn

object Test {
  def main(args: Array[String]): Unit = {
    val b1 = new Bippy1(71)
    val b2 = new Bippy2(71)
    assert(b1 == b1 && b1.## == b1.x.##, ((b1, b1.##)))
    assert(b2 == b2 && b2.## == b2.x.##, ((b2, b2.##)))
  }
}
