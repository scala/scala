
object Test {
  def main(args: Array[String]): Unit = {
    val c = new C()
    val x = c.d
    assert(x.toString == "ok")
  }
}
/* Because scalac used wrong class D in C.java, was:
 * java.lang.NoSuchFieldError: d
 *   at Test$.main(Test.scala:5)
 *   at Test.main(Test.scala)
 */
