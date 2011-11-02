/** This doesn't test that the optimization is working, only that
 *  nothing is exploding.
 */
object Test {
  def f1(x: AnyRef)      = "" + x
  def f2(x: Int)         = "" + x
  def f3(x: Array[Char]) = "" + x
  def f4(x: List[Int])   = "" + x
  def f5(x: Any)         = "" + x
  def f6(x: AnyVal)      = "" + x
  
  def main(args: Array[String]): Unit = {
    List(f1("a"), f2(5), f3(null), f3(Array('a')), f4(List(1)), f5(null), f6(55d)) mkString ""
  }
}
