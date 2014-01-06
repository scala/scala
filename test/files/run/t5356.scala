
import scala.language.{ reflectiveCalls }
object Test {
  def f(x: Any { def toInt: Int }) = println(x.toInt + " " + x.getClass.getName)

  def main(args: Array[String]): Unit = {
    f(1)
    f(1.toInt)
    f(BigInt(1))
    f(1d)
    f(1f)
    println((1: (Any { def toInt: Int })).toInt)
  }
}
