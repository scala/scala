object Const { final val VAL = 1 ; final val VAR = 2 }
import Const._
object Test {
  def test(i: Int) = i match { case v @ (VAR | VAL) => v == VAR case _ => "default" }
  def main(args: Array[String]): Unit = {
    println(test(VAR))
    println(test(VAL))
    println(test(-1))
  }
}
