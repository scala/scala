// Testing that isEmpty and get are viewed with `memberType` from `Casey1`.
trait T[A, B >: Null] { def isEmpty: A = false.asInstanceOf[A]; def get: B = null}
class Casey1() extends T[Boolean, String]
object Casey1 { def unapply(a: Casey1) = a }

object Test {
  def main(args: Array[String]) {
    val c @ Casey1(x) = new Casey1()
    assert(x == c.get)
  }
}
