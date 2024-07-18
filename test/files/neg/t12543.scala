
//> using options -Werror -release 8

import scala.jdk.CollectionConverters._
//import jdk.CollectionConverters._  // scala/bug/issues/12566
import sun._

object HelloWorld {
  def main(args: Array[String]) = {
    val ss = java.util.List.of("Hello", who)
    println(ss.asScala.mkString("", ", ", "!"))
  }
}

object sun {
  val who = "world"
}
