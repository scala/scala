// scalac: -Ydelambdafy:method-ref
import java.io.File

import scala.collection.mutable

object Test {
  val fqnsToFiles = mutable.HashMap[String, (File, Boolean)]()

  def main(args: Array[String]): Unit = test()

  def test() = {
    val fqn = "bob"
    val newResult = Option((new File("f"), true))
    newResult.foreach(res => fqnsToFiles.put(fqn, res))
  }
}
