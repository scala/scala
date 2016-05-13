
case class AffineImageShape(axes: Seq[Int]) {
  def this(axes: Array[Int]) = this(axes)
}
class X(i: Int) {
  def this(d: Double) = this(d.toLong)
  def this(n: Long) = this(n.toInt)
}
