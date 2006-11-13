
/** Test that unboxing and promotion (from int to double) work together.
 *  Was bug 891.
 */
object Test {
  def main(args:Array[String]):Unit = {
    Console println
    List(Pair(1,2.0)).map[double]({ x  => x match {
      case Pair(a, x) => Console.println("B"); x * a
    }});
  }
}
