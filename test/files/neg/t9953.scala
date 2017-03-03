
class X(val v: Int) extends AnyVal
trait T extends Any
object Y extends T

class C {
  val x = new X(42)
  val y = new Object
  val a: T = null
  def b = y == x   // warn
  def c = y == a   // no warn
  def d = Y == a   // no warn
}
