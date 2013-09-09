
import PartialFunction.cond
import util._

object Test extends App {
  val days = (1 to 12).toList

  days map s"On the $_th day of Christmas" foreach println

  val rf = (n: Int) => s"\\*{$_}"(n).r
  def stars(n: Int)(s: String) = {
    val r = rf(n)
    cond(s) { case r(_*) => true }
  }
  Console println stars(5)("*****")

  days zip days map s"${_: Int} by ${_: Int}".tupled foreach println
}
