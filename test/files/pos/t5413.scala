/* scalac: -Xsource:3.0 */

object Fail {
  def nom (guard : => Boolean) (something : => Unit): Unit = { }
  def main(args: Array[String]): Unit = {
    nom {
      val i = 0
      (i != 3)
    }(())
  }
}
