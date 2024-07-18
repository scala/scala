//> using options -Werror -Wunused:implicits
//

trait InterFace {
  /** Call something. */
  def call(a: Int, b: String, c: Double)(implicit s: String): Int
}

trait BadAPI extends InterFace {
  def f(a: Int,
        b: String,
        c: Double
       )(implicit s: String): Int = {  // warn
    println(b + c)
    a
  }
  @deprecated ("no warn in deprecated API", since="yesterday")
  def g(a: Int,
        b: String,
        c: Double
       )(implicit s: String): Int = {  // no warn
    println(b + c)
    a
  }
  override def call(a: Int,
                    b: String,
                    c: Double
                   )(implicit s: String): Int = {  // no warn, required by superclass
    println(b + c)
    a
  }

  def i(implicit s: String, t: Int) = t           // yes, warn
}

trait T {
  def f()(implicit i: Int): Int
}
trait U { _: T =>
  override def f()(implicit i: Int): Int = g()  // no warn, required by baseclass, scala/bug#12876
  def g(): Int
}

trait CanEqual[A, B]
trait BadCanEqual[A, B] extends Runnable

class EqTest {
  implicit class ArrowAssert[A](a: A) {
    def ==>[B](b: B)(implicit ev: CanEqual[A, B]): Boolean = a == b // no warn, no non-trivial members
  }
  implicit class BadArrowAssert[A](a: A) {
    def ==>[B](b: B)(implicit ev: BadCanEqual[A, B]): Boolean = a == b // warn, ev.run
  }
}

trait MembersOnly[A] {
  private def member() = 42 // don't care whether member is accessible; maybe they must pass it elsewhere
}
class Privately {
  def f[A](implicit m: MembersOnly[A]) = toString.nonEmpty  // warn implicit trait with private member
}
