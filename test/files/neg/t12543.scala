
// scalac: -Werror -release:8

import scala.collection.JavaConverters._
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
