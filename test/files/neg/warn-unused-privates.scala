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
}

class A(val msg: String)
class B1(msg: String) extends A(msg)
class B2(msg0: String) extends A(msg0)
class B3(msg0: String) extends A("msg")

/*** Early defs full of noise due to SI-6595. ***/
/***
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
***/

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

trait DefaultArgs {
  // warn about default getters for x2 and x3
  private def bippy(x1: Int, x2: Int = 10, x3: Int = 15): Int = x1 + x2 + x3

  def boppy() = bippy(5, 100, 200)
}
