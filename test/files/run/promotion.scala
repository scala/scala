
/** Test that unboxing and promotion (from int to double) work together.
 *  Was bug 819.
 */
object Test {

  def id[A](x: A): A = x;
  def main(args: Array[String]): Unit = {
    Console.println(id(1) * 2.0)
    Console.println(3.0 * id(2))
    Console.println(id(4.0) * 5)
    Console.println(6 * id(5.0))
  }
}
