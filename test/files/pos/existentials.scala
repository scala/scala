/** All of these should work, some don't yet.
 *  !!!
 */
class A {
  def f() = { case class Bob(); Bob }

  val quux0 = f()
  def quux1 = f()
  // lazy val quux2 = f()
  // def quux3 = {
  //   lazy val quux3a = f()
  //   quux3a
  // }

  val bippy0 = f _
  def bippy1 = f _
  // lazy val bippy2 = f _
  // val bippy3 = {
  //   lazy val bippy3a = f _
  //   bippy3a
  // }
}
