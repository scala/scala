class Bippy(a: Int, b: Int) {
  private def this(c: Int) = this(c, c)           // warn
  private def bippy(x: Int): Int      = bippy(x)  // TODO: could warn
  private def boop(x: Int)            = x+a+b     // warn
  final private val MILLIS1           = 2000      // no warn, might have been inlined
  final private val MILLIS2: Int      = 1000      // warn
  final private val HI_COMPANION: Int = 500       // no warn, accessed from companion
  def hi() = Bippy.HI_INSTANCE
}
object Bippy {
  def hi(x: Bippy) = x.HI_COMPANION
  private val HI_INSTANCE: Int = 500      // no warn, accessed from instance
  private val HEY_INSTANCE: Int = 1000    // warn
  private lazy val BOOL: Boolean = true   // warn
}

class A(val msg: String)
class B1(msg: String) extends A(msg)
class B2(msg0: String) extends A(msg0)
class B3(msg0: String) extends A("msg")

/*** Early defs warnings disabled primarily due to scala/bug#6595.
 *   The test case is here to assure we aren't issuing false positives;
 *   the ones labelled "warn" don't warn.
 ***/
class Boppy extends {
  private val hmm: String = "abc"       // no warn, used in early defs
  private val hom: String = "def"       // no warn, used in body
  private final val him   = "ghi"       // no warn, might have been (was) inlined
  final val him2          = "ghi"       // no warn, same
  final val himinline     = him
  private val hum: String = "jkl"       // warn
  final val ding = hmm.length
} with Mutable {
  val dinger = hom
  private val hummer = "def" // warn

  private final val bum   = "ghi"       // no warn, might have been (was) inlined
  final val bum2          = "ghi"       // no warn, same
}

trait Accessors {
  private var v1: Int = 0 // warn
  private var v2: Int = 0 // warn, never set
  private var v3: Int = 0 // warn, never got
  private var v4: Int = 0 // no warn

  def bippy(): Int = {
    v3 = 5
    v4 = 6
    v2 + v4
  }
}

class StableAccessors {
  private var s1: Int = 0 // warn
  private var s2: Int = 0 // warn, never set
  private var s3: Int = 0 // warn, never got
  private var s4: Int = 0 // no warn

  def bippy(): Int = {
    s3 = 5
    s4 = 6
    s2 + s4
  }
}

trait DefaultArgs {
  // warn about default getters for x2 and x3
  private def bippy(x1: Int, x2: Int = 10, x3: Int = 15): Int = x1 + x2 + x3

  def boppy() = bippy(5, 100, 200)
}

/* scala/bug#7707 Both usages warn default arg because using PrivateRyan.apply, not new.
case class PrivateRyan private (ryan: Int = 42) { def f = PrivateRyan() }
object PrivateRyan { def f = PrivateRyan() }
*/

class Outer {
  class Inner
}

trait Locals {
  def f0 = {
    var x = 1 // warn
    var y = 2
    y = 3
    y + y
  }
  def f1 = {
    val a = new Outer // no warn
    val b = new Outer // warn
    new a.Inner
  }
  def f2 = {
    var x = 100 // warn about it being a var
    x
  }
}

object Types {
  private object Dongo { def f = this } // warn
  private class Bar1 // warn
  private class Bar2 // no warn
  private type Alias1 = String // warn
  private type Alias2 = String // no warn
  def bippo = (new Bar2).toString

  def f(x: Alias2) = x.length

  def l1() = {
    object HiObject { def f = this } // warn
    class Hi { // warn
      def f1: Hi = new Hi
      def f2(x: Hi) = x
    }
    class DingDongDoobie // warn
    class Bippy // no warn
    type Something = Bippy // no warn
    type OtherThing = String // warn
    (new Bippy): Something
  }
}

trait Underwarn {
  def f(): Seq[Int]

  def g() = {
    val Seq(_, _) = f()  // no warn
    true
  }
}

class OtherNames {
  private def x_=(i: Int): Unit = ???
  private def x: Int = 42
  private def y_=(i: Int): Unit = ???
  private def y: Int = 42

  def f = y
}

case class C(a: Int, b: String, c: Option[String])
case class D(a: Int)

trait Boundings {

  def c = C(42, "hello", Some("world"))
  def d = D(42)

  def f() = {
    val C(x, y, Some(z)) = c              // warn
    17
  }
  def g() = {
    val C(x @ _, y @ _, Some(z @ _)) = c  // no warn
    17
  }
  def h() = {
    val C(x @ _, y @ _, z @ Some(_)) = c  // warn for z?
    17
  }

  def v() = {
    val D(x) = d                          // warn
    17
  }
  def w() = {
    val D(x @ _) = d                      // warn, fixme (valdef pos is different)
    17
  }

}

trait Forever {
  def f = {
    val t = Option((17, 42))
    for {
      ns <- t
      (i, j) = ns                        // no warn
    } yield (i + j)
  }
  def g = {
    val t = Option((17, 42))
    for {
      ns <- t
      (i, j) = ns                        // warn, fixme
    } yield 42                           // val emitted only if needed, hence nothing unused
  }
}

trait Ignorance {
  private val readResolve = 42      // ignore
}

trait CaseyKasem {
  def f = 42 match {
    case x if x < 25 => "no warn"
    case y if toString.nonEmpty => "no warn" + y
    case z => "warn"
  }
}
trait CaseyAtTheBat {
  def f = Option(42) match {
    case Some(x) if x < 25 => "no warn"
    case Some(y @ _) if toString.nonEmpty => "no warn"
    case Some(z) => "warn"
    case None => "no warn"
  }
}

class `not even using companion privates`

object `not even using companion privates` {
  private implicit class `for your eyes only`(i: Int) {  // warn
    def f = i
  }
}

class `no warn in patmat anonfun isDefinedAt` {
  def f(pf: PartialFunction[String, Int]) = pf("42")
  def g = f {
    case s => s.length        // no warn (used to warn case s => true in isDefinedAt)
  }
}
