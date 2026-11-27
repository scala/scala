//> using options -nowarn
trait A; trait B
trait U1 extends Any; trait U2 extends Any
class AB extends A with B
object Test {
  def nullAB: A with B = null
  def nullU: U1 with U2 = null
  def get(x: A with B): A with B = { println("get"); x }
  def main(args: Array[String]): Unit = {
    val ab: A with B = new AB
    println(ab.isInstanceOf[A with B])                    // true
    println(nullAB.isInstanceOf[A with B])                // false: null is not an instance of anything
    println(get(null).isInstanceOf[A with B])             // false
    println((null: AB).isInstanceOf[A with B])            // false
    // controls: one parent, or a static type that does not already conform
    println((null: AB).isInstanceOf[A])                   // false
    println((nullAB: Any).isInstanceOf[A with B])         // false
    println(nullAB match { case _: A with B => true; case _ => false }) // false
    println(nullU.isInstanceOf[U1 with U2])               // false: universal traits do not rule out null
    println((null: Null).isInstanceOf[A with B])          // false
    val x = nullAB
    try { if (x.isInstanceOf[A with B]) println(x.hashCode) else println("not an instance") } catch { case e: NullPointerException => println("NPE inside the guarded branch") }
  }
}
