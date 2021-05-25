
object Test extends App {
  val xs =
    Array.empty[Double]
  val ys =
    Array(0.0)
  assert(xs.intersect(ys).getClass.getComponentType == classOf[Double])
  assert(Array.empty[Double].intersect(Array(0.0)).getClass.getComponentType == classOf[Double])
}
