import scala.tools.partest.IcodeTest

object Test extends IcodeTest {
  override def code =
    """object Case3 {                                // 01
     |  def unapply(z: Any): Option[Int] = Some(-1)  // 02
     |  def main(args: Array[String]) {              // 03
     |    ("": Any) match {                          // 04
     |      case x : String =>                       // 05 Read: <linenumber> JUMP <target basic block id>
     |        println("case 0")                      // 06 expecting "6 JUMP 7", was "8 JUMP 7"
     |      case _ =>                                // 07
     |        println("default")                     // 08 expecting "8 JUMP 7"
     |    }                                          // 09
     |    println("done")                            // 10
     |  }
     |}""".stripMargin

  override def show() {
    val lines1 = collectIcode("")
    println(lines1 mkString "\n")
  }
}
