
trait MapI[C] {
  def i: Int
  def s: String
  def copy(i: Int = this.i, s: String = this.s): C
  def mapI(i: Int): C = copy(i)
}

case class C(i: Int, s: String) extends MapI[C]

/*
was:
t12623.scala:9: error: class C needs to be abstract.
Missing implementation for member of trait MapI:
  def copy(i: Int, s: String): C = ???

case class C(i: Int, s: String) extends MapI[C]
           ^
 */
