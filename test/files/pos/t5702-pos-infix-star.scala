
object Test {
  case class *(a: Int, b: Int)
  type Star = *
  case class P(a: Int, b: Star) // alias still required

  def main(args: Array[String]) {
    val v = new *(6,7)
    val x * y = v
    printf("%d,%d\n",x,y)
    val p = P(5, v)
    val P(a, b * c) = p
    printf("%d,%d,%d\n",a,b,c)
  }
}
