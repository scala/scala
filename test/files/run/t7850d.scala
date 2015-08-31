// Testing that the ad-hoc overload resolution of isEmpty/get discards
// parameter-accepting variants
trait T[A, B >: Null] { def isEmpty: A = false.asInstanceOf[A]; def get: B = null}
class Casey1(val a: Int) {
  def isEmpty: Boolean = false
  def isEmpty(x: Int): Boolean = ???
  def get: Int = a
  def get(x: Int): String = ???
}
object Casey1 { def unapply(a: Casey1) = a }

object Test {
  def main(args: Array[String]) {
    val c @ Casey1(x) = new Casey1(0)
    assert(x == c.get)
  }
}
