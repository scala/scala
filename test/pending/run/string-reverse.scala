/** In case we ever feel like taking on unicode string reversal.
 *  See ticket #2565.
 */
object Test {
  val xs = "Les Mise\u0301rables"   // this is the tricky one to reverse
  val ys = "Les Misérables"
  val xs2 = new StringBuilder(xs)
  val ys2 = new StringBuilder(ys)
  
  def main(args: Array[String]): Unit = {
    val out = new java.io.PrintStream(System.out, true, "UTF-8")
    
    out.println("Strings")
    List(xs, xs.reverse, ys, ys.reverse) foreach (out println _)
    
    out.println("StringBuilder")
    out.println(xs2.toString)
    out.println(xs2.reverseContents().toString)
    out.println(ys2.toString)
    out.println(ys2.reverseContents().toString)
  }
}